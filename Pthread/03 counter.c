/* compile with GCC using "gcc -pthread  simple_example.c*/

/* This program should take 7 seconds to run even though it requests
a total of 28 seconds of sleep. Each of the sleep commands starts at
the same time and runs in parallel so you're just waiting for the 7
seconds of the longest single command*/

#define NITS 10000

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

int counter = 0;

/*Store a mutual exclusion object. This has to be global because
it has to be common between all threads but needs to be accessed
in the thread functions*/
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void* testthread(void* arg){

  int i;
  int counter_local = 0;
  /*Do a local count and then add it. It's a good idea to do a little
  work as possible inside the mutual exclusion lock*/
  for (i = 0; i<NITS ;++i){
    counter_local++;
  }

  pthread_mutex_lock(&mutex);
  counter += counter_local;
  pthread_mutex_unlock(&mutex);

  return NULL;
}

int main (int argc, char ** argv){

  int i;

  /*Store the thread IDs and the thread return values*/
  pthread_t mythreads[8];
  void* rvals[8];

  /*Loop 8 times creating threads*/
  for (i = 0;i<8;++i){
    pthread_create(&mythreads[i], NULL, testthread, NULL);
  }

  /*This waits for the threads to finish*/
  for (i = 0;i<8;++i){
    pthread_join(mythreads[i], rvals+i);
  }

  /*The counter doesn't work because the threads are all trying to update
  the same variable and they interfere with each other*/
  printf("Counter value is %i. Should be %i\n",counter,8*NITS);

}
