#!/bin/bash -l
#SBATCH --time 01:00:00
#SBATCH --nodes 16
#SBATCH --constraint=mc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mplaneta@os.inf.tu-dresden.de
#SBATCH --partition=normal
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-core=2
#SBATCH --exclusive
#SBATCH --output=slurm-%j.out
#SBATCH --error=slurm-%j.err

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
COMBINATIONS=""
TYPES=Corrected
ITERATION=1000
MSG_SIZE_MIN="8"
MSG_SIZE_MAX="256"
MSG_SIZE="$MSG_SIZE_MIN:$MSG_SIZE_MAX"
CORRT_COUNT_MAX=$MSG_SIZE_MAX
# Extract from TASKS_PER_NODE from SLURM_TASKS_PER_NODE (e.g. "72(x4)" -> "72")
TASKS_PER_NODE=$(echo $SLURM_TASKS_PER_NODE | sed 's/\(.*\)(.*).*/\1/g;s/[^0-9]//g')
NNODES="$SLURM_JOB_NUM_NODES"
REPETITION="10"
CORRT_GOSSIP_ROUNDSS=1
# We what this variable to show in the logs
export CORRT_GOSSIP_SEEDS=$RANDOM

#Binomial tree

CORRT_DISS_TYPE=tree_binomial
TREE_LAME_KS=1
CORRT_DISTS="{0,2,4}"
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults
CORRT_DISTS="4"
FAULTS="72"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

#Lame tree

CORRT_DISS_TYPE=tree_lame
CORRT_DISTS="{0,2,4}"
TREE_LAME_KS="{2,4}"
FAULTS="0"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults
CORRT_DISTS="4"
FAULTS="72"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# Baseline without shared memory
CORRT_DISS_TYPE=tree_binomial
TREE_LAME_KS=1
TYPES=Native
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# Baseline with shared memory
CORRT_DISS_TYPE=tree_binomial
TREE_LAME_KS=1
TYPES=Best
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# Our broadcast with optimal rank ordering
CORRT_DISS_TYPE=tree_binomial
TYPES=Mapping
CORRT_DISTS="0"
FAULTS="0"

COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# Opportunistic gossip

# COMBINATIONS=""
TYPES=Corrected
CORRT_DISS_TYPE=gossip
CORRT_GOSSIP_SEEDS=23
CORRT_GOSSIP_ROUNDSS="{22,24,25,26}"
CORRT_DIST="{2,4}"
FAULTS="0"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

# With faults
FAULTS="72"

# COMBINATIONS="$COMBINATIONS "$(eval echo "$CORRT_DISS_TYPE+$TREE_LAME_KS+$TYPES+$NNODES+$CORRT_DISTS+$FAULTS+$CORRT_GOSSIP_SEEDS+$CORRT_GOSSIP_ROUNDSS")

cd $WORKDIR

# Size         Avg Latency(us)     Min Latency(us)     Max Latency(us)  Iterations
echo "Type	Corr	Nnodes	Size	Avg	Min	Max	Iterations	Rep	Faults	GossipSeed	GossipRounds"

OUTDIR=$(date "+%d%m%y_%H-%M-%S".$RANDOM)
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
	read CORRT_DISS_TYPE TREE_LAME_K TYPE NNODES CORRT_DIST FAULT CORRT_GOSSIP_SEED CORRT_GOSSIP_ROUNDS <<<$(IFS="+"; echo $EXPERIMENT)
	export NPROC=$(($TASKS_PER_NODE*$NNODES))
	
	OUTFILE="$OUTDIR/$EXPERIMENT+$TASKS_PER_NODE+$NPROC+$i"
	# Rank zero may never die
	DYING_LIST=($(shuf -i 1-$(($NPROC - 1)) -n $FAULT))
	# DYING_LIST=($(seq 1 $FAULT))
	DYING_LIST=$(IFS=';'; echo "${DYING_LIST[*]}")

	rm MPICH_RANK_ORDER
	export DYING_LIST
	export CORRT_DIST
	export CORRT_COUNT_MAX
	export CORRT_DISS_TYPE
	export TREE_LAME_K
	export CORRT_GOSSIP_SEED
	export CORRT_GOSSIP_ROUNSD
	EXPORT="--export=DYING_LIST=$DYING_LIST,CORRT_DIST=$CORRT_DIST,CORRT_COUNT_MAX=$CORRT_COUNT_MAX,CORRT_DISS_TYPE=$CORRT_DISS_TYPE,TREE_LAME_K=$TREE_LAME_K,CORRT_GOSSIP_SEED=$CORRT_GOSSIP_SEED,CORRT_GOSSIP_ROUNDS=$CORRT_GOSSIP_ROUNDS"
	case "$TYPE" in
	    'Corrected')
		EXPORT="$EXPORT,LD_PRELOAD=$DYING_LIB,MPICH_RANK_REORDER_METHOD=0,MPICH_SHARED_MEM_COLL_OPT=0"
		;;
	    'Mapping')
		EXPORT="$EXPORT,LD_PRELOAD=$DYING_LIB,MPICH_RANK_REORDER_METHOD=3,MPICH_SHARED_MEM_COLL_OPT=0"
		cp $BASE/mapping/MPICH_RANK_ORDER.$NNODES ./MPICH_RANK_ORDER
		;;
	    'Best')
		# Do nothing
		;;
	    'Native')
		# Do nothing
		EXPORT="$EXPORT,MPICH_SHARED_MEM_COLL_OPT=0"
		;;
	esac

	echo "$DYING_LIST" > $OUTFILE
	echo "$EXPERIMENT"

	echo "srun $EXPORT --cpu_bind=core -n $NPROC $OSU_DIR/osu_bcast -m $MSG_SIZE -f -i $ITERATION 2>/dev/null"
	OUT=$(srun $EXPORT --cpu_bind=core -n $NPROC $OSU_DIR/osu_bcast -m $MSG_SIZE -f -i $ITERATION 2>/dev/null)
	echo "$OUT"
	echo "$OUT"  | tail -n +3 >> $OUTFILE
    done
done

echo $OUTDIR
