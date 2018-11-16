#pragma once

/*
 * Setup an in-order binomial tree similar to what MPICH provides
 *
 * Returns MPI status code
 */
int
setup_tree_binomial_in_order(int const rank, int const comm_size,
                             size_t *num_child, size_t *parent, size_t **children);
