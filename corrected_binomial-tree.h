#pragma once

/*
 * Setup a binomial tree
 *
 * Returns MPI status code
 */
int
setup_tree_binomial(int const rank, int const comm_size,
                    size_t *num_child, size_t *parent, size_t **children);
