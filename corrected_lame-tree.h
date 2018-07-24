#pragma once


/*
 * Setup a lame tree
 *
 * Returns 'true' on success, 'false' on error
 */
bool
setup_tree_lame(int const rank, int const comm_size,
                size_t *num_child, size_t *parent, size_t **children);
