#include<stdio.h>
#include<stdlib.h>
#include<omp.h>

#define MAX_ITS 10000

int main(int argc, char **argv)
{

  int i, it;

/*OpenMP uses { } in C rather than begin and end markers*/
#pragma omp parallel private(it)
{
  it = 0;
#pragma omp for
  for (i = 0; i< MAX_ITS; ++i){
    it++;
  }
  printf("Processor %i performed %i iterations\n", omp_get_thread_num(), it);
}
}
