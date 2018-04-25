#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>

#include "dying.h"

static MPI_Comm MPI_COMM_LIVING_WORLD;
int will_die = 0;

void die_if_needed()
{
  int rank;

  PMPI_Comm_rank(MPI_COMM_WORLD, &rank);

  char *dying_list = getenv("DYING_LIST");

  if (!dying_list) {
    fprintf(stderr, "DYING_LIST is unset: Nobody is going to die\n");
    PMPI_Abort(MPI_COMM_WORLD, -1);
  }

  /* Traverse dying_list, check oneself */
  int dying_rank = -1;
  while (1) {
    int ret = sscanf(dying_list, "%d", &dying_rank);
    if (dying_rank == rank && ret) {
      will_die = 1;
      break;
    }
    dying_list = strchr(dying_list, ',');
    /* Can't find a comma, hence the end of the list */
    if (!dying_list) break;

    dying_list++;
  }

  /* Create two communicators. New world for all operations except the
     broadcast. The old comm world will be used for the corrected
     broadcast. */
  int color = !!will_die;
  int ret = PMPI_Comm_split(MPI_COMM_WORLD, color, rank, &MPI_COMM_LIVING_WORLD);

  if (ret != MPI_SUCCESS) {
    fprintf(stderr, "Failed to split communicator @ %d\n", rank);
    PMPI_Abort(MPI_COMM_WORLD, -1);
  }

  if (will_die) {
    PMPI_Finalize();
    exit(0);
    /* not reached */
  }
}

void dying_finalize()
{
  int rank;
  PMPI_Comm_rank(MPI_COMM_WORLD, &rank);
  fflush(stdout);
  fflush(stderr);
  PMPI_Abort(MPI_COMM_SELF, 0);
  /* kill(getpid(), SIGTERM); */
}

/* Replace a new COMM_WORLD with an old one, where some nodes are
   potentially dead. */
MPI_Comm replace_comm_world(MPI_Comm comm)
{
  if (comm == MPI_COMM_WORLD) {
    return MPI_COMM_LIVING_WORLD;
  }

  return comm;
}

int die_in_bcast()
{
  if (will_die) {
    PMPI_Finalize();
    exit(0);
    /* not reached */
    return 1;
  }

  return 0;
}
