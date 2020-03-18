PROGRAM counter

  !This solves the counter problem by using active OpenMP commands. In this
  !version I use OpenMP commands to get the number of threads and a unique ID
  !number for each thread. I then allocate an array large enough for each
  !thread to accumulate its own total in and then add the array up in serial
  !at the end

  !Note that to do this I'm having to use omp_lib to get the
  !runtime OpenMP commands
  USE omp_lib
  IMPLICIT NONE
  INTEGER :: i, thread_num
  INTEGER, DIMENSION(:), ALLOCATABLE :: cvar

  ALLOCATE(cvar(omp_get_max_threads()))
  cvar = 0

!Here I have split the OpenMP region into a PARALLEL section and a 
!DO section rather than a PARALLEL DO. This is only because it lets
!me call omp_get_thread_num once rather than on each iteration 
!$OMP PARALLEL PRIVATE(i, thread_num)
  !OpenMP gives threads IDs starting from 0 but we want starting from 1
  thread_num = omp_get_thread_num() + 1
!$OMP DO
  DO i = 1, 1000
    cvar(thread_num) = cvar(thread_num) + 1
  END DO
!$OMP END DO
!$OMP END PARALLEL

  PRINT *, SUM(cvar)

END PROGRAM counter
