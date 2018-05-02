#!/bin/bash
#SBATCH --time 00:10:00
#SBATCH --nodes 18
#SBATCH --mail-type=ALL
#SBATCH --mail-user=maksym.planeta@tu-dresden.de
#SBATCH --partition=haswell64
#SBATCH --mem-per-cpu 2000
#SBATCH --ntasks-per-node 24
#SBATCH --exclusive

export MODULEPATH=~s9951545/.modules:$MODULEPATH

module add ompi/3.0.1rc4-opt gcc/7.1.0 parallel 2>/dev/null

SCRIPTDIR="$( cd "$( dirname "$0" )" && pwd )"
BASEDIR=$SCRATCH/corrected-mpi
DYING_LIB=$BASEDIR/dying/build/libdying.so

WORKDIR=$BASEDIR/osu-micro-benchmarks-5.4.1/mpi/collective

ITERATIONS=1000
MSG_SIZE="256"
CORR_COUNT_MAX="$MSG_SIZE"
# Extract from TASKS_PER_NODE from SLURM_TASKS_PER_NODE (e.g. "72(x4)" -> "72")
TASKS_PER_NODE=$(echo $SLURM_TASKS_PER_NODE | sed 's/\(.*\)(.*).*/\1/g;s/[^0-9]//g')
REPETITION="7"

# Fault free case
NNODES="{18,36,72}"
NNODES="18"
ALGORITHMS="7"
CORR_DISTS="{0,2}"
FAULTS="0"

COMBINATIONS=$(eval echo "$ALGORITHMS+$CORR_DISTS+$NNODES+$FAULTS")

ALGORITHMS="6"
CORR_DISTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$CORR_DISTS+$NNODES+$FAULTS")

ALGORITHMS="Native"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$CORR_DISTS+$NNODES+$FAULTS")

# Add faults
ALGORITHMS="7"
CORR_DISTS="{2,4}"
FAULTS="{1,2}"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$CORR_DISTS+$NNODES+$FAULTS")

cd $WORKDIR

# Size         Avg Latency(us)     Min Latency(us)     Max Latency(us)  Iterations
echo "Algorithm	Nnodes	Size	Avg	Min	Max	Iterations	Rep	Faults"

OUTDIR="$BASEDIR/logs/$(date "+%H-%M-%S_%d%m%y".$RANDOM).taurus"
mkdir -p $OUTDIR
echo $OUTDIR

for NNODES in $(eval echo $NNODES)
do
    mpiexec hostname | sort -u | tail -n $NNODES > $OUTDIR/hostfile.$NNODES
done

cat $SCRIPTDIR/$0 > $OUTDIR/script.sh
env > $OUTDIR/script.env

for i in $(seq 1 $REPETITION)
do
    for EXPERIMENT in $COMBINATIONS
    do
	read ALGORITHM CORR_DIST NNODES FAULT <<<$(IFS="+"; echo $EXPERIMENT)
	export NPROC=$(($TASKS_PER_NODE*$NNODES))
	
	OUTFILE="$OUTDIR/$EXPERIMENT+$i"
	# Rank zero may never die
	DYING_LIST=($(shuf -i 1-$(($NPROC - 1)) -n $FAULT))
	# DYING_LIST=($(seq 1 $FAULT))
	DYING_LIST=$(IFS=';'; echo "${DYING_LIST[*]}")

	export DYING_LIST
	export CORR_DIST
	export CORR_COUNT_MAX
	EXPORT="-x DYING_LIST=$DYING_LIST -x CORR_DIST=$CORR_DIST -x CORR_COUNT_MAX=$CORR_COUNT_MAX"
	if [[ "$ALGORITHM" == 'Native' ]] ; then
	    EXPORT="$EXPORT --mca pml yalla"
	else
	    EXPORT="$EXPORT --mca pml ob1 --mca coll_tuned_bcast_algorithm $ALGORITHM"
	    EXPORT="$EXPORT --mca coll_tuned_use_dynamic_rules 1"
	    EXPORT="$EXPORT -x LD_PRELOAD=$DYING_LIB"
	fi


	echo "$ALGORITHM	$CORR_DIST	$NPROC	$NNODES	$i	$DYING_LIST" > $OUTFILE
	echo "$EXPERIMENT"

	OUT=$(mpiexec $EXPORT -np $NPROC --map-by core --bind-to core \
		      bash -c "ulimit -s 10240; $WORKDIR/osu_bcast -m $MSG_SIZE -f -i $ITERATIONS" 2>/dev/null)
	echo "$OUT"
	echo "$OUT" | grep -v WARN | tail -n +3 >> $OUTFILE
    done
done


