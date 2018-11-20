`dying-trees` is a mixture of 1) a failure emulator and 2) an implementation
of corrected broadcast algorithms that can use either Gossip or trees in
their dissemination. The library is designed to interpose MPI calls with
the help of the PMPI interface. Calls to MPI_Bcast will then be replaced
by a call to the library's own broadcast.

"Dead nodes" are emulated by not calling the broadcast function for them.
Thus they do not participate in the algorithm at all. All other
(non-fault-aware) MPI functions are called normally by all ranks. Not doing
so would result in a deadlock as the MPI library is not able to handle
faults and would wait indefinitely for the missing ranks.


# Requirements
- CMake >= 3.1
- Python
- Compiler with support for C99
- MPI library


# Compilation

```bash
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make
```

# Usage

The library is designed for being preloaded to an MPI application via the
`LD_PRELOAD` mechanism. It is controlled by various environment variables:

- `CORRT_DIST` — correction distance
- `CORRT_COUNT_MAX` — maximum supported messge size
- `CORRT_DISS_TYPE` — dissemination type
  - `gossip` — (hop-based) Gossip, based on hops instead of time (see paper)
  - `tree_binomial` — interleaved binomial tree
  - `tree_lame` — interleaved Lamé tree
  - `tree_binomial_in_order` — non-interleaved binomial tree
- `CORRT_GOSSIP_SEED` — random seed used to determine Gossip partners
                        (not used for the tress)
- `CORRT_GOSSIP_ROUNDS` — number of hops a message travels in Gossip
                          (Gossip is not limited by time but by this value)
- `TREE_LAME_K` — order of the Lamé tree (only used with Lamé tree)

- `DYING_LIST` — comma-seperated list of ranks that "should die"


## Example usage

```bash
srun --export=DYING_LIST='',CORR_DIST=2,CORR_COUNT_MAX=256,TREE_TYPE=lame,TREE_LAME_K=2,LD_PRELOAD=./build/libdying.so -n 48 ./osu-micro-benchmarks-5.4.1/mpi/collective/osu_bcast -m 256 -f -i 1000
```
