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
/*Only one processor at a time can be in the critical section
All the others have to queue up to enter this
If you code spends long in critical sections then it will be barely
and faster than if it is running on a single processor*/
#pragma omp critical
    {
    its_global++;
    }
  }
}

printf( "Counter records %i iterations\n", its_global);
}
