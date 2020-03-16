#include <stdio.h>
#include <stdlib.h>
#define NUMS 400000000
#define MAXITS 1000


int main(int argc, char ** argv)
{
  int *counts = NULL;
  int64_t iint, itrial, icurr, ct;


  /*Allocate arrays for the numbers to test the Collatz conjecture on
  Counts contains the number of iterations required to reach 1 */
  counts = malloc(sizeof(int) * NUMS);
  /*Set all counts values to -1 as a sentinal. If a value stays -1 it means
  that there was no convergence */
  for (iint = 0; iint < NUMS; ++iint){
    counts[iint] = -1;
  }
  /*Loop over all numbers to start the sequence from*/
  for (iint = 0; iint < NUMS; ++iint){
    icurr = iint + 1; /*Capture the value to start from*/
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
  for (iint = 0; iint < NUMS; ++iint){
    if (counts[iint] < 0) ct++;
  }
  printf("Number of non-converged items %i\n", ct);
}
