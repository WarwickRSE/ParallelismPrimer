#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv)
{
  int errcode;
/*All C MPI functions return their error codes. Here I'm only trapping the first
one as demonstration. Error handling in MPI is a bit messy and generally nearly
impossible to recover from so a lot of people don't bother trapping MPI errors.
By default any MPI error will stop your code immediately.
*/
  errcode = MPI_Init(&argc, &argv);
  printf("Multiprocessor code\n");
  MPI_Finalize();

}
