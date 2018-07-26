#pragma once

#include <stdbool.h>

/*
 * Setup a binomial tree
 *
 * Returns 'true' on success, 'false' on error
 */
bool
setup_tree_binomial(int const rank, int const comm_size,
                    size_t *num_child, size_t *parent, size_t **children);
