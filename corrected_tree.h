#pragma once

int
ompi_coll_base_bcast_intra_corrected(void *buff, int count,
                                     MPI_Datatype datatype,
                                     int root,
                                     MPI_Comm comm,
                                     void *module);
#define REPLACE_COMM_WORLD(comm) comm = replace_comm_world(comm);

