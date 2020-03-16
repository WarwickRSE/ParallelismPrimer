/* compile with GCC using "gcc -pthread  simple_example.c*/

/* This program should take 7 seconds to run even though it requests
a total of 28 seconds of sleep. Each of the sleep commands starts at
the same time and runs in parallel so you're just waiting for the 7
seconds of the longest single command*/

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

void* testthread(void* arg){

  int i =*(int*)arg;
  printf("Sleeping for %i\n", i);
  sleep(i);
  return NULL;

}

int main (int argc, char ** argv){

  int i;
  /*Threads need to have their own variables so define them as arrays here
  can allocate memory and pass it in too but this is not needed for an example*/
  int ival[8];

  /*Also create arrays to hold the thread IDs and the return values*/
  pthread_t mythreads[8];
  void* rvals[8];

  /*Loop 8 times creating threads*/
  for (i = 0;i<8;++i){
    /*pthread_create returns immediately but the thread takes time
    to start so you have to make sure that you don't change the value
    that is passed to the thread before the thread runs. Here, use an
    array*/
    ival[i]=i;
    pthread_create(&mythreads[i], NULL, testthread, &ival[i]);
  }

  /*This waits for the threads to finish*/
  for (i = 0;i<8;++i){
    pthread_join(mythreads[i], rvals+i);
  }

}
