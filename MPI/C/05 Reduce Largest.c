#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{

  int nproc, rank, rank_red;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

/*MPI_Reduce combines values from all processors. Here it finds the maximum
  value (MPI_MAX) over all processors. It gets that value on one processor
  called the "root" processor, here rank 0. The related MPI_Allreduce gives the
  reduced value to all processors*/

  MPI_Reduce(&rank, &rank_red, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
  if (rank == 0) printf("Largest rank is %i\n", rank_red);

  /*MPI_Allreduce combines values from all processors. Here it finds the sum of
  the value (MPI_SUM) over all processors. It gets that value on all 
  processors*/
  MPI_Allreduce(&rank, &rank_red, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  if (rank == nproc-1) printf("Sum of ranks is %i\n", rank_red);

  MPI_Finalize();

}
