#include <assert.h>
#include <mpi.h>
#include <stdlib.h>

#include "corrected_binomial-tree.h"
#include "util.h"


/* Find MSB in a given number
 *
 * Approach taken from simulator
 */
static size_t
get_msb(int num)
{
    int msb = 0;

    // Find most significant bit
    while (num) {
        num /= 2;
        ++msb;
    }
    return msb;
}


int
setup_tree_binomial(int const rank, int const comm_size,
                    size_t *num_child, size_t *parent, size_t **children)
{
    size_t const len_rank = get_msb(rank),          // length/bits of rank ID
                 len_tree = get_msb(comm_size - 1); // max length/bits of rank IDs

    *num_child = len_tree - len_rank; // remaining bits = (max) children
    *parent    = rank ? rank & ~(1 << (len_rank-1)) // drop MSB of rank ID to find our parent
                      : (comm_size + 1); // "obviously faulty" value for root node
    assert(*num_child >= 0 && "Negative child count");

    // no children -> no allocation
    if (!*num_child) { return CORRT_ERR_SUCCESS; }

    // prepare child array
    *children = malloc( *num_child * sizeof(size_t) );
    if (!*children) { return CORRT_ERR_NO_MEM; }

    // Child ranks are found by setting each unused bit above our MSB in turn
    for (size_t cc=0; cc < *num_child; ++cc) {
        assert(len_rank + cc + 1 <= len_tree && "Too many children");

        (*children)[cc] = rank + (1 << (len_rank + cc));

        // handle non-full tree
        if ((*children)[cc] >= (unsigned const)comm_size) {
            *num_child = cc; // remember the new count (and implicitly exit loop)
            *children = realloc(*children, sizeof(size_t) * *num_child); // trim child list (optional memory optimisation)
            assert(*children && "Out of memory while *shrinking* array");
        }
    }
    return CORRT_ERR_SUCCESS;
}
