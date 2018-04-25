#pragma once

#include <mpi.h>

void die_if_needed();
void dying_finalize();
int die_in_bcast();
MPI_Comm replace_comm_world(MPI_Comm comm);


int
ompi_coll_base_bcast_intra_corrected(void *buff, int count,
                                     MPI_Datatype datatype,
                                     int root,
                                     MPI_Comm comm);
#define REPLACE_COMM_WORLD(comm) comm = replace_comm_world(comm);

#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#include "dying_wrap.h"
#pragma GCC diagnostic warning "-Wdeprecated-declarations"
