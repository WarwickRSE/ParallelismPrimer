#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#define NUMS 400000000
#define MAXITS 1000


int main(int argc, char ** argv)
{
  int *counts = NULL;
  int nproc, rank, err;
  int64_t iint, itrial, icurr, ct, nums_local, lower, ct_global;

  err = MPI_Init(&argc, &argv);
  err = MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  err = MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  nums_local = NUMS / (int64_t)nproc;

  lower = (int64_t)rank * nums_local;

  /*Allocate arrays for the numbers to test the Collatz conjecture on
  Counts contains the number of iterations required to reach 1 */
  counts = malloc(sizeof(int) * NUMS);
  /*Set all counts values to -1 as a sentinal. If a value stays -1 it means
  that there was no convergence */
  for (iint = 0; iint < NUMS; ++iint){
    counts[iint] = -1;
  }
  /*Loop over all numbers to start the sequence from*/
  for (iint = 0; iint < nums_local; ++iint){
    icurr = iint + 1 + lower; /*Capture the value to start from*/
    /*Loop to a maximum number of iterations
    You can check if any are not converged after MAXITS by checking for
    the sentinal value of -1 in count*/
    for (itrial = 1; itrial <= MAXITS; ++itrial){
      /*If icurr is even divide by two*/
      if (icurr%2 == 0) {
        icurr = icurr / 2;
      } else {
        /*Otherwise multiply by 3 and add 1*/
        icurr = icurr * 3 + 1;
      }
      /*If the number reaches 1 then sequence has converged*/
      if (icurr == 1) {
        counts[iint] = itrial;
        break;
      }
    }
  }

  ct = 0;
  for (iint = 0; iint < nums_local; ++iint){
    if (counts[iint] < 0) ct++;
  }
  err = MPI_Reduce(&ct, &ct_global, 1, MPI_INT64_T, MPI_SUM, 0, MPI_COMM_WORLD);
  if (rank == 0) printf("Number of non-converged items %i\n", ct_global);
  err = MPI_Finalize();
}
