#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <mpi.h>

#define ISPRIME 2
#define ISCOMP 1
#define ISUNCHECKED 0

#define SMALL_NUM 20

const long small_primes[SMALL_NUM] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71};
const long max_small_prime = 71;

char check_prime(long num);


  void dispatcher_fn(long lower, long upper, long len, char* flag_array)
    {

    int nproc, worker_num;
    MPI_Status stat;
    long next_package_start;
    char result;
    long offset, dummy_dat;
    long * current_packages;

    double * start_times, * end_times, * cum_times;
    long * packages_processed;

    // Number of packages currently out
    int inflight;

    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    // Keep track of what each worker is working on
    current_packages = (long *) calloc(nproc, sizeof(long));

    //These track how long each package takes, and cumulative totals per worker
    start_times = (double *) malloc(nproc*sizeof(double));
    end_times = (double *) malloc(nproc*sizeof(double));
    cum_times = (double *) calloc(nproc, sizeof(double));
    packages_processed = (long *) calloc(nproc, sizeof(long));


    // Index of next lowest number to check. When this hits len we are done
    next_package_start = 0;
    inflight = 0;

    for(;;) {

      //Recieve and unpack the results
      
      //Tag is used for operational info - is this a ready/done signal, or a result?
      
      //Wait to receive any data. On the first pass tag will be 0 which is
      //Just the workers saying that they are ready
      MPI_Recv(&result, 1, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG,
          MPI_COMM_WORLD, &stat);

      //We have to get this information from the status variable because
      //we received the message with MPI_ANY_SOURCE and MPI_ANY_TAG
      if (stat.MPI_TAG > 0) {
        //Capture end time and add to this workers stats
        end_times[stat.MPI_SOURCE-1] = MPI_Wtime();
        cum_times[stat.MPI_SOURCE-1] += end_times[stat.MPI_SOURCE-1] - start_times[stat.MPI_SOURCE-1];
        packages_processed[stat.MPI_SOURCE-1] ++;
        //Package recieved and understood
        inflight --;
        //Now set the correct flag for this package
        offset = current_packages[stat.MPI_SOURCE-1] - lower;
        flag_array[offset] = result;
      }

      //If you've still got work to give out then find the next candidate and dispatch
      if (next_package_start < len) {

        //Now find the next candidate and send it to the worker that just reported
        //that it had finished.
        //The tag is "1" indicating this is real work

        for(;;){
          //Skip over numbers already tested
          if( flag_array[next_package_start] == 0) break;
          next_package_start ++;
          if(next_package_start > len) break;
        }

        current_packages[stat.MPI_SOURCE-1] = lower + next_package_start;
        next_package_start ++;

        inflight ++;
        //Record the start time
        start_times[stat.MPI_SOURCE-1] = MPI_Wtime();

        //Send the work package with the tag of the work index to the source
        //of the previously received message
        MPI_Send(&current_packages[stat.MPI_SOURCE-1], 1, MPI_LONG, stat.MPI_SOURCE,
            1, MPI_COMM_WORLD);
      }
      if(inflight == 0) break;
    }

    //After there are no more inflight messages send the shutdown message to all workers
    for (worker_num = 1; worker_num < nproc; ++worker_num){
      //No more work to do so shut down the worker. Here, this is just
      //Sending a message with a zero tag
      dummy_dat = -1;
      MPI_Send(&dummy_dat, 1, MPI_LONG, worker_num, 0, MPI_COMM_WORLD);
    }

    for(worker_num = 0; worker_num < nproc-1; ++worker_num){
      printf("Worker %3i checked %3li primes in %5.4g seconds\n",
          worker_num+1, packages_processed[worker_num], cum_times[worker_num]);
    }
  }


  void worker_fn(void)
    {
    MPI_Status stat;
    long package, tag;
    char dat, result;
    double start_time, wait_time;

    //First just call home to say "I'm here and I'm waiting"
    //So send -1 message and 0 tag to rank 0
    dat = -1;
    MPI_Send(&dat, 1, MPI_CHAR, 0, 0, MPI_COMM_WORLD);

    for(;;){
      //Receive the work package from rank 0
      MPI_Recv(&package, 1, MPI_LONG, 0, MPI_ANY_TAG,
          MPI_COMM_WORLD, &stat);

      //Need to have a termination condition, here tag = 0
      if (stat.MPI_TAG == 0) break;

      //Do the work
      result = check_prime(package);

      //Send the result back using the same tag
      MPI_Send(&result, 1, MPI_CHAR, 0 , stat.MPI_TAG, MPI_COMM_WORLD);
    }
  }

  char check_prime(long num){

    char result;
    long index, end;
    
    end = ceil(sqrt((double) num));

    result = ISPRIME;

    //First remember that 1 is not considered prime
    if (num == 1) return ISCOMP;

    //Then check against the small primes
    for(index = 0; index < SMALL_NUM; index++){
      //If the number is one of the small primes then it is prime
      if (num == small_primes[index]) return ISPRIME;
      //If it is a multiple of one of the small primes then it is not prime
      if (num%small_primes[index] == 0){
        return ISCOMP;
      }
    }
    
    //Test higher numbers, skipping all the evens
    //Only need to test to SQRT(num) because this is the largest possible
    //prime factor
    for (index = max_small_prime + 2; index <= end; index += 2){
      if (num%index == 0){
        return ISCOMP;
      }
    }
    return result;
  }

int main(int argc, char** argv)
{
  int rank, nproc, i, total;
  long lower_bound, upper_bound, len;
  char * flags;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc == 1) {
    printf("This example requires more than 1 processor\n");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }
  if (argc != 3) {
    printf("Please supply the ends of range to check");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }

  lower_bound = atoi(argv[1]);  upper_bound = atoi(argv[2]);
  len = upper_bound - lower_bound;

  if (len <= 0) {
    printf("Lower bound must be < upper bound");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }

  //Array of flags corresponding to numbers to test
  // Flag is 0 for unchecked, ISCOMP for composite and ISPRIME for prime
  flags = (char *) calloc(len, sizeof(char));

  if (rank == 0) {
    dispatcher_fn(lower_bound, upper_bound, len, flags);
    total = 0;
    for( i=0; i < len; i++){
      if(flags[i] == ISPRIME){
        //printf("%li, ", lower_bound+i);
        total ++;
      }
    }
    printf("\nFound %i primes\n", total);
  } else {
    worker_fn();
  }
  
  MPI_Finalize();
  
  return 0;
}

