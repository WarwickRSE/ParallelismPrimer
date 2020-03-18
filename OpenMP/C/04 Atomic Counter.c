#include<stdio.h>
#include<stdlib.h>
#include<omp.h>

#define MAX_ITS 10000

int main(int argc, char **argv)
{

  int i, its_global;

its_global = 0;
#pragma omp parallel
{
#pragma omp for
  for (i = 0; i< MAX_ITS; ++i){
/*This tells the compiler that this code should be atomic
That means that individual operations are guaranteed to complete
in a way that can't be interrupted by other threads*/
#pragma omp atomic update
    its_global++;
  }
}

printf( "Counter records %i iterations\n", its_global);
}
