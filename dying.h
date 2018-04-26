#pragma once

#include <mpi.h>

#include "corrected_tree.h"

void die_if_needed();
void dying_finalize();
int die_in_bcast();
MPI_Comm replace_comm_world(MPI_Comm comm);


#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#include "dying_wrap.h"
#pragma GCC diagnostic warning "-Wdeprecated-declarations"
