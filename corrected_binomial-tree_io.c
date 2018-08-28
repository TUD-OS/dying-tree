#include <assert.h>
#include <mpi.h>
#include <stdlib.h>

#include "corrected_binomial-tree_io.h"
#include "util.h"


/* concept from MPICH ('mpi/coll/ibcast.c') */
int
setup_tree_binomial_in_order(int const rank, int const comm_size,
                             size_t *num_child, size_t *parent, size_t **children)
{
    // find parent
    int mask = 0x1; // lsb
    *num_child = 0;

    while (mask < comm_size) {
        if (rank & mask) {
            assert(*parent == 0xBEEFBABEul && "Two parents!?");
            *parent = rank - mask;
            assert(*parent >= 0 && "Broken tree");
            break;
        }
        mask <<= 1;
        ++*num_child;
    }

    if (!rank) {
        assert(mask >= comm_size);
        assert(*parent == 0xBEEFBABE && "Root got a parent!?");
        *parent = comm_size + 1; // "obviously faulty" value for root node
    }

    // find children
    mask >>= 1;
    *children = malloc( *num_child * sizeof(size_t) );
    if (!*children) { return CORRT_ERR_NO_MEM; }


    // Child ranks are found by setting each unused bit below our LSB in turn
    size_t cc = 0;
    while (mask > 0) {
        assert(cc < *num_child && "Too many children");

        if (rank + mask < comm_size) {
            (*children)[cc] = rank + mask;
        }
        else {
            // handle non-full tree
            --*num_child; // remember the new count
            *children = realloc(*children, sizeof(size_t) * *num_child); // trim child list (optional memory optimisation)
            assert((!*num_child || *children) && "Out of memory while *shrinking* array");
        }
        mask >>= 1;
        ++cc;
    }
    return CORRT_ERR_SUCCESS;
}
