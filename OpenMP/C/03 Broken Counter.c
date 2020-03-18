#include<stdio.h>
#include<stdlib.h>
#include<omp.h>

#define MAX_ITS 10000

int main(int argc, char **argv)
{

  int i, its_global;

/*
This will give the wrong counter result
This is because adding to the counter involves three operations
1) Get the current value
2) Increment it
3) Put it back
In serial you don't worry about this, but in parallel what happens
if two processors get the current value at the same time? They both increment
it separately and put back a value which is 1 greater than it was before
This means that this counter will be
1) Lower than the true value
2) Random because the error only occurs when two thread happen to try to
increment the counter at the same time
*/

#pragma omp parallel
{
  its_global = 0;
#pragma omp for
  for (i = 0; i< MAX_ITS; ++i){
    its_global++;
  }
}

printf( "Counter records %i iterations\n", its_global);
}
