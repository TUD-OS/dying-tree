#include <assert.h>
#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>

#include "corrected_gossip.h"
#include "util.h"


int
setup_gossip(int const rank, int const comm_size,
             size_t *num_child, size_t *num_parent,
             size_t **parents,  size_t **children)
{
    if (comm_size < 3) {
        fprintf(stderr, "Gossip needs at least 3 ranks ... nobody sends to "
                        "himself and nobody sends to root\n");
        return CORRT_ERR_WRONG_ARG;
    }

    srandom(read_env_int("CORRT_GOSSIP_SEED")); // init ramdomness, same for all ranks!

    int const i_rounds = read_env_int("CORRT_GOSSIP_ROUNDS");
    assert(i_rounds > 0 && "Invalid number of Gossip rounds");
    size_t const rounds = (size_t)i_rounds;

    // we will have 'rounds' children and up to 'rounds * (comm_size - 1)'
    // parents in (the worst possible) case that everybody else sends only to us
    *num_child = rounds;
    *children  = malloc( *num_child * sizeof(size_t) );

    *num_parent = rounds * (comm_size - 1);
    *parents    = malloc( *num_parent * sizeof(size_t) );
    if (!*children || !*parents) { return CORRT_ERR_NO_MEM; }

    // figure out who our relatives are
    //
    // Generate (pseudo-randomly) *all* children of *all* nodes until we
    // found all our children and parents
    size_t cur_parent = 0;
    for (size_t cur_rank = 0; cur_rank < comm_size; ++cur_rank) {
        for (size_t cur_child = 0; cur_child < rounds; ++cur_child) {
            int candidate;

            // find suitable relationships
            do {
                candidate = random() % comm_size;
            } while (cur_rank == candidate || candidate == 0);

            // our children
            if (cur_rank == rank) {
                (*children)[cur_child] = candidate;
                continue;
            }

            // our parents
            if (candidate == rank) {
                assert(cur_parent < *num_parent && "Too many parents");
                (*parents)[cur_parent++] = cur_rank;
            }
        }
    }

    // handle non-worst cases :-)
    if (cur_parent < *num_parent) {
        *num_parent = cur_parent;
        *parents = realloc(*parents, sizeof(size_t) * *num_parent); // trim parent list (optional memory optimisation)
        assert( (*parents || !*num_parent) && "Out of memory while *shrinking* array");
    }

    return CORRT_ERR_SUCCESS;
}
