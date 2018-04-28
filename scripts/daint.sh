#!/bin/bash -l
#SBATCH --time 00:30:00
#SBATCH --nodes 24
#SBATCH --constraint=mc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mplaneta@os.inf.tu-dresden.de
#SBATCH --partition=normal
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=2
#SBATCH --exclusive

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


## Fault free case
TYPES=Corrected
ITERATION=1000
MSG_SIZE="2048"
CORR_COUNT_MAX=$MSG_SIZE
# Extract from TASKS_PER_NODE from SLURM_TASKS_PER_NODE (e.g. "72(x4)" -> "72")
TASKS_PER_NODE=$(echo $SLURM_TASKS_PER_NODE | sed 's/\(.*\)(.*).*/\1/g;s/[^0-9]//g')
NNODES="$SLURM_JOB_NUM_NODES"
REPETITION="10"

CORR_DISTS="{0,8}"
FAULTS="0"

COMBINATIONS=$(eval echo "$TYPES+$NNODES+$CORR_DISTS+$FAULTS")

# Faults
CORR_DISTS="8"
FAULTS="4"

COMBINATIONS="$COMBINATIONS "$(eval echo "$TYPES+$NNODES+$CORR_DISTS+$FAULTS")

# Baseline
TYPES=Native
CORR_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$TYPES+$NNODES+$CORR_DISTS+$FAULTS")

cd $WORKDIR

# Size         Avg Latency(us)     Min Latency(us)     Max Latency(us)  Iterations
echo "Type	Corr	Nnodes	Size	Avg	Min	Max	Iterations	Rep	Faults"

OUTDIR=$(date "+%H-%M-%S_%d%m%y".$RANDOM)
mkdir $OUTDIR
echo $OUTDIR

for NNODES in $(eval echo $NNODES)
do
    srun hostname | sort -u | tail -n $NNODES > $OUTDIR/hostfile.$NNODES
done

cat $0 > $OUTDIR/script.sh
env > $OUTDIR/script.env

for i in $(seq 1 $REPETITION)
do
    for EXPERIMENT in $COMBINATIONS
    do
	read TYPE NNODES CORR_DIST FAULT <<<$(IFS="+"; echo $EXPERIMENT)
	export NPROC=$(($TASKS_PER_NODE*$NNODES))
	
	OUTFILE="$OUTDIR/$EXPERIMENT+$i"
	# Rank zero may never die
	DYING_LIST=($(shuf -i 1-$(($NPROC - 1)) -n $FAULT))
	DYING_LIST=$(IFS=';'; echo "${DYING_LIST[*]}")

	export DYING_LIST
	export CORR_DIST
	export CORR_COUNT_MAX
	EXPORT="--export=DYING_LIST=$DYING_LIST,CORR_DIST=$CORR_DIST,CORR_COUNT_MAX=$CORR_COUNT_MAX"
	if [[ "$TYPE" == 'Corrected' ]] ; then
	    EXPORT="$EXPORT,LD_PRELOAD=$DYING_LIB"
	fi

	echo "$TYPE	$CORR_DIST	$NPROC	$NNODES	$i	$DYING_LIST" > $OUTFILE
	echo "$EXPERIMENT"

	OUT=$(srun $EXPORT --cpu_bind=core -n $NPROC -x 0 \
		   $OSU_DIR/osu_bcast -m $MSG_SIZE -f -i $ITERATION)
	echo "$OUT"
	echo "$OUT"  | tail -n +3 >> $OUTFILE
    done
done

echo $OUTDIR
