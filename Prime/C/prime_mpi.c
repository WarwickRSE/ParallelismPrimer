#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#define MAXVAL 20000001

//In real working code these should be done more carefully
// to ensure no inconsistency in values
const long small_primes_len = 20;
const long small_primes[20] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71};
const long max_small_prime = 71;

char check_prime(long num);

  char check_prime(long num){

    char result;
    long index, end;

    end = ceil(sqrt((double) num));

    result = 1;
    //First check against the small primes
    for(index = 0; index < small_primes_len; index++){
      if (small_primes[index] == num) return 1;
      if (num%small_primes[index] == 0){
        return 0;
      }
    }

    //Test higher numbers, skipping all the evens
    for (index = max_small_prime + 2; index <= end; index += 2){
      if (num%index == 0){
        return 0;
      }
    }
    return result;
  }

int main(int argc, char **argv)
{
  long i, ct, nums_per_proc, local_min, local_max, ct_global;
  int nproc, rank, err;

  err = MPI_Init(&argc, &argv);

  err = MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  err = MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  nums_per_proc = (MAXVAL-1) / (long)nproc;
  local_min = 2 + (long)rank * nums_per_proc;
  local_max = 1 + (long)(rank + 1) * nums_per_proc;

  ct = 0;
  for (i = local_min; i<=local_max; ++i){
    if (check_prime(i))ct = ct + 1;
  }

  err = MPI_Reduce(&ct, &ct_global, 1, MPI_LONG, MPI_SUM, 0, MPI_COMM_WORLD);

  if (rank == 0) printf("Number of primes = %i\n", ct_global);

  err = MPI_Finalize();
}
