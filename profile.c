#include <time.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>

#include <mpi.h>

#define ITERATIONS 10000L
#define USEC 1000L

int corrected_broadcast(void *const, int const, MPI_Datatype const, int const, MPI_Comm const);

int my_rank;
int numprocs;

uint64_t get_time()
{
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  return (uint64_t)ts.tv_sec * 1000000000L + ts.tv_nsec;
}

void print_stat(const uint64_t time)
{
  uint64_t min_time, max_time, avg_time;

  MPI_Reduce(&time, &min_time, 1, MPI_UINT64_T, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&time, &max_time, 1, MPI_UINT64_T, MPI_MAX, 0, MPI_COMM_WORLD);
  MPI_Reduce(&time, &avg_time, 1, MPI_UINT64_T, MPI_SUM, 0, MPI_COMM_WORLD);

  // Compute time per iteration in microseconds
  min_time = min_time / (ITERATIONS * USEC);
  max_time = max_time / (ITERATIONS * USEC);
  avg_time = avg_time / (ITERATIONS * USEC * (uint64_t)numprocs);

  if (!my_rank) {
    printf("%" PRIu64 "\t%" PRIu64 "\t%" PRIu64 "\n", min_time, max_time, avg_time);
  }
  
}

int main(int argc, char **argv)
{
  putenv("CORRT_COUNT_MAX=256");
  putenv("CORRT_DISS_TYPE=tree_binomial");
  putenv("CORRT_DIST=0");

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

  if (!my_rank) {
    printf("%d\n", numprocs);
    printf("Min\tMax\tAvg\n");
  }
  
  static char buffer[4096];
  int size = 256;

  // Warmup
  for (int i = 0; i < ITERATIONS; i++) {
    MPI_Bcast(buffer, size, MPI_CHAR, 0, MPI_COMM_WORLD);
    MPI_Barrier(MPI_COMM_WORLD);
  }

  uint64_t t_start, t_stop, t_elapsed = 0;

  // Our bcast
  for (int i = 0; i < ITERATIONS; i++) {
    t_start = get_time();
    corrected_broadcast(buffer, size, MPI_CHAR, 0, MPI_COMM_WORLD);
    t_stop = get_time();

    t_elapsed += t_stop - t_start;
    MPI_Barrier(MPI_COMM_WORLD);
  }

  print_stat(t_elapsed);

  // Normal bcast
  t_elapsed = 0;
  for (int i = 0; i < ITERATIONS; i++) {
    t_start = get_time();
    MPI_Bcast(buffer, size, MPI_CHAR, 0, MPI_COMM_WORLD);
    t_stop = get_time();

    t_elapsed += t_stop - t_start;
    MPI_Barrier(MPI_COMM_WORLD);
  }

  print_stat(t_elapsed);

  MPI_Finalize();
}
