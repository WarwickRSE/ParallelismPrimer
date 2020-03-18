#include<stdio.h>
#include<mpi.h>

int main(int argc, char** argv)
{
  int rank, nproc;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  printf("I am processor %i of %i\n", rank+1, nproc);
  MPI_Finalize();
}
