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

module add ompi/3.0.1rc4-opt parallel GCC/7.3.0-2.30 2>/dev/null

SCRIPTDIR="$( cd "$( dirname "$0" )" && pwd )"
BASEDIR=$HOME/corrected-mpi
DYING_LIB=$BASEDIR/dying/build/libdying.so

WORKDIR=$BASEDIR/osu-micro-benchmarks-5.4.1/mpi/collective

ITERATIONS=1000
MSG_SIZE="256"
CORRT_COUNT_MAX="$MSG_SIZE"
# Extract from TASKS_PER_NODE from SLURM_TASKS_PER_NODE (e.g. "72(x4)" -> "72")
TASKS_PER_NODE=$(echo $SLURM_TASKS_PER_NODE | sed 's/\(.*\)(.*).*/\1/g;s/[^0-9]//g')
REPETITION="2"
CORRT_GOSSIP_SEEDS=23
CORRT_GOSSIP_ROUNDSS=1
NNODES="{18,36,72}"
NNODES="18"

############# Measuring normal binomial tree

# Fault free case
ALGORITHMS="7"
CORRT_DISS_TYPE=tree_binomial
CORRT_DISTS="{0,2}"
FAULTS="0"

COMBINATIONS=$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults

FAULTS="{1,2}"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

############# Measuring gossip

# Fault free case
ALGORITHMS="7"
CORRT_DISS_TYPE=gossip
CORRT_DISTS="{0,2}"
CORRT_GOSSIP_SEEDS=23
CORRT_GOSSIP_ROUNDSS=25
FAULTS="0"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults

FAULTS="{1,2}"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

############# Measuring the overhead of the wrapper

# Fault free case
ALGORITHMS="Wrapper"
CORRT_DISS_TYPE=tree_binomial
CORRT_DISTS="{0,2}"
FAULTS="0"

COMBINATIONS=$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults

FAULTS="{1,2}"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

############# Baseline comparison with libdying

ALGORITHMS="6"
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

############# Baseline comparison without libdying

ALGORITHMS="Native"
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$ALGORITHMS+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

cd $WORKDIR

# Size         Avg Latency(us)     Min Latency(us)     Max Latency(us)  Iterations
echo "Algorithm	Nnodes	Size	Avg	Min	Max	Iterations	Rep	Faults	GossipSeed	GossipRounds"

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
	read ALGORITHM NNODES CORRT_DIST FAULT CORRT_GOSSIP_SEED CORRT_GOSSIP_ROUNDS <<<$(IFS="+"; echo $EXPERIMENT)
	export NPROC=$(($TASKS_PER_NODE*$NNODES))
	
	OUTFILE="$OUTDIR/$EXPERIMENT+$TASKS_PER_NODE+$NPROC+$i"
	# Rank zero may never die
	DYING_LIST=($(shuf -i 1-$(($NPROC - 1)) -n $FAULT))
	# DYING_LIST=($(seq 1 $FAULT))
	DYING_LIST=$(IFS=';'; echo "${DYING_LIST[*]}")

	export DYING_LIST
	export CORRT_DIST
	export CORRT_COUNT_MAX
	EXPORT="-x DYING_LIST=$DYING_LIST -x CORRT_DIST=$CORRT_DIST -x CORRT_COUNT_MAX=$CORRT_COUNT_MAX -x CORRT_GOSSIP_SEED=$CORRT_GOSSIP_SEED -x CORRT_GOSSIP_ROUNDS=$CORRT_GOSSIP_ROUNDS"
	if [[ "$ALGORITHM" == 'Native' ]] ; then
	    EXPORT="$EXPORT --mca pml yalla"
	elif [[ "$ALGORITHM" == 'Wrapper' ]] ; then
	    EXPORT="$EXPORT --mca pml yalla"
	    EXPORT="$EXPORT -x LD_PRELOAD=$DYING_LIB"
	else
	    EXPORT="$EXPORT --mca pml ob1 --mca coll_tuned_bcast_algorithm $ALGORITHM"
	    EXPORT="$EXPORT --mca coll_tuned_use_dynamic_rules 1"
	    EXPORT="$EXPORT -x LD_PRELOAD=$DYING_LIB"
	fi


	echo "$DYING_LIST" > $OUTFILE
	echo "$EXPERIMENT"

	OUT=$(mpiexec $EXPORT -np $NPROC --map-by core --bind-to core \
		      bash -c "ulimit -s 10240; $WORKDIR/osu_bcast -m $MSG_SIZE -f -i $ITERATIONS" 2>/dev/null)
	echo "$OUT"
	echo "$OUT" | grep -v WARN | tail -n +3 >> $OUTFILE
    done
done


