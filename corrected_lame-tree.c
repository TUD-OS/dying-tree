#include <assert.h>
#include <mpi.h>
#include <stdlib.h>

#include "corrected_lame-tree.h"
#include "util.h"


// all setup_tree_lame_* functions taken from simulator (and adopted to C)
static int
setup_tree_lame_ready_to_send(int const t, int const k)
{
    if (t < 0) { return 0; }
    if (t < k) { return 1; }

    size_t idx = t - k;

    // No STL available ... so implement our own simple std::vector
    static size_t cache_size     = 0;    // number of entries
    static size_t cache_capacity = 0;    // size of 'values' array
    static int   *cache_values   = NULL; // malloc'd content array

    // required value not cached yet
    while (idx >= cache_size) {
        unsigned const new_t     = cache_size + k;
        int      const new_rts_1 = setup_tree_lame_ready_to_send(new_t - 1, k);
        int      const new_rts_2 = setup_tree_lame_ready_to_send(new_t - k, k);

        if (new_rts_1 < 0 || new_rts_2 < 0) { return -1; }

        // need more space?
        if (cache_size == cache_capacity) {
            cache_capacity += k; // arbitrary value that seems to make sense :-)
            cache_values = realloc(cache_values, sizeof(int) * cache_capacity);
            if (!cache_values) { return -1; }
        }

        cache_values[cache_size] = new_rts_1 + new_rts_2;
        ++cache_size;
    }

    assert(cache_values[idx] > 0 && "rts(t) always > 0 if t>=0");
    return cache_values[idx];
}


static int
setup_tree_lame_start(int const id, int const k)
{
    if (id == 0) { return 0; }

    for (int t = 0; /* no test */; t++) {
        if (setup_tree_lame_ready_to_send(t, k) > id) { return t; }
    }

    return -1; // here be dragons
}


int
setup_tree_lame(int const rank, int const comm_size,
                size_t *num_child, size_t *parent, size_t **children)
{
    int const k = read_env_int("TREE_LAME_K");
    int parent_tmp = rank ? -1 : (comm_size + 1); // obvious fake parent for root, invalid one for others

    *num_child = 0;

    // Note: We could stop after looking at our own rank, but let's do all
    //       so we can check consistency...
    for (int sender = 0; sender < comm_size; ++sender) {
        int lvl = setup_tree_lame_start(sender, k);
        if (lvl < 0) { return CORRT_ERR_WRONG_ARG; }

        while (1) {
            int const rts = setup_tree_lame_ready_to_send(lvl + k - 1, k);
            if (rts < 0) { return CORRT_ERR_WRONG_ARG; }

            int const receiver = sender + rts;
            if (receiver >= comm_size) { break; }

            // found our parent node
            if (rank == receiver) {
                assert(parent_tmp < 0 && "There can be only one ... parent");
                parent_tmp = sender;
                break; // we don't care who our siblings are
            }

            // found one of our children
            if (rank == sender) {
                ++(*num_child);

            // prepare/enlarge child array
            // TODO: better allocation would be nice ... setup is a one-shot function though
                *children = realloc(*children, *num_child * sizeof(size_t) );
                if (!*children) { return CORRT_ERR_NO_MEM; }

                (*children)[*num_child - 1] = receiver;
            }
        ++lvl;
        }
    }

    assert(parent_tmp >= 0 && "No parent found");
    *parent = parent_tmp;
	 return CORRT_ERR_SUCCESS;
}
