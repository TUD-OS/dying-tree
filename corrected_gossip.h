#pragma once

/*
 * Setup (pseudo) Gossip
 *
 * Returns MPI status code
 */
int
setup_gossip(int const rank, int const comm_size,
             size_t *num_child, size_t *num_parent,
             size_t **parents,  size_t **children);
