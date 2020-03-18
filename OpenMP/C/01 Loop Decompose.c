#include<stdio.h>
#include<stdlib.h>
#include<omp.h>

#define MAX_ITS 10000

int main(int argc, char **argv)
{

  int nproc, thread_id, i, it_total;
  int *its_per_proc;

  nproc = omp_get_max_threads();
  its_per_proc = calloc(nproc, sizeof(int));
#pragma omp parallel for
  for (i = 0; i< MAX_ITS; ++i){
    thread_id = omp_get_thread_num();
    its_per_proc[thread_id] ++;
  }
/*No end here like in Fortran. Only for parallel for*/

  it_total = 0;
  for (i = 0; i< nproc; ++i){
    printf("Processor %i performed %i iterations\n", i, its_per_proc[i]);
    it_total += its_per_proc[i];
  }

  printf("Total work on all processors is %i\n", it_total);
}
