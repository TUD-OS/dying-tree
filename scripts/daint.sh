#!/bin/bash -l
#SBATCH --time 00:30:00
#SBATCH --nodes 24
#SBATCH --constraint=mc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mplaneta@os.inf.tu-dresden.de
#SBATCH --partition=normal
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=1

module load daint-mc

BASE=$SCRATCH/corrected-mpi

DYING_LIB=$BASE/dying-tree/build/libdying.so
DYING_LIB_TMP=/tmp/$USER/$(basename $DYING_LIB)

WORKDIR=$BASE/logs
OSU_DIR=$BASE/osu-micro-benchmarks-5.4.1/mpi/collective/

# Copy libdying to local storage

# srun mkdir -p /tmp/$USER/
# srun --ntasks-per-node 1 rm -f $DYING_LIB_TMP
# echo cp from $DYING_LIB to $DYING_LIB_TMP
# srun --ntasks-per-node 1 cp -f $DYING_LIB $DYING_LIB_TMP

# Extract from TASKS_PER_NODE from SLURM_TASKS_PER_NODE (e.g. "72(x4)" -> "4")
TASKS_PER_NODE=$(echo $SLURM_TASKS_PER_NODE | sed 's/.*(\(.*\)).*/\1/g;s/[^0-9]//g')

# Fault free case
ITERATION=1000
MSG_SIZE="256"
CORR_COUNT_MAX=$MSG_SIZE
NNODES="{1,12,24}"
CORR_DISTS="{0,2,4,8}"
REPETITIONS="3"
FAULTS="0"

COMBINATIONS=$(eval echo "$NNODES+$CORR_DISTS+$REPETITIONS+$FAULTS")

# Faults
CORR_DISTS="{2,4,8}"
REPETITIONS="3"
FAULTS="{1,2}"

COMBINATIONS="$COMBINATIONS "$(eval echo "$NNODES+$CORR_DISTS+$REPETITIONS+$FAULTS")

cd $WORKDIR

# Size         Avg Latency(us)     Min Latency(us)     Max Latency(us)  Iterations
echo "Corr	Nnodes	Size	Avg	Min	Max	Iterations	Rep	Faults"

OUTDIR=$(date "+%H-%M-%S_%d%m%y")
mkdir $OUTDIR

for NNODES in $(eval echo $NNODES)
do
    mpiexec hostname | sort -u | tail -n $NNODES | awk '{print $1" slots=24"}' > $OUTDIR/hostfile.$NNODES
done

for EXPERIMENT in $COMBINATIONS
do
    for i in $(seq 1 $REPETITION)
    do
	read NNODES CORR_DIST REPETITION FAULT <<<$(IFS="+"; echo $EXPERIMENT)
	export NPROC=$(($TASKS_PER_NODE*$NNODES))
	
	OUTFILE="$OUTDIR/$EXPERIMENT+$i"
	# Rank zero may never die
	DYING_LIST=($(shuf -i 1-$(($NPROC - 1)) -n $FAULT))
	DYING_LIST=$(IFS=','; echo "${DYING_LIST[*]}")

	export DYING_LIST
	export CORR_DIST
	export CORR_COUNT_MAX
	OUT=$(srun --export=LD_PRELOAD="$DYING_LIB" \
		   -n $NPROC \
		   $OSU_DIR/osu_bcast -m $MSG_SIZE -f -i $ITERATION)
	echo "$EXPERIMENT"
	echo "$OUT"
	echo "$CORR_DIST	$NPROC	$i	$DYING_LIST" > $OUTFILE
	echo "$OUT"  | tail -n +3 >> $OUTFILE
    done
done
