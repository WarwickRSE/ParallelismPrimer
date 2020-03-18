#include<stdio.h>
#include<stdlib.h>
#include<omp.h>

#define MAX_ITS 10000

int main(int argc, char **argv)
{

  int i, its_global;

its_global = 0;
#pragma omp parallel reduction(+:its_global)
{
#pragma omp for
  for (i = 0; i< MAX_ITS; ++i){
    its_global++;
  }
}

printf( "Counter records %i iterations\n", its_global);
}
