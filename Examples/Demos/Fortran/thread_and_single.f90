PROGRAM thread_and_single

  USE omp_lib
  IMPLICIT NONE

  PRINT *, 'Hello from outside parallel section'

!$OMP PARALLEL
  !Every thread will print this one
  PRINT *,'Hello from thread'
!They will then all wait at this line
!$OMP BARRIER

!Only one thread will enter this section
!By default all threads will wait until the thread entering this section
!finishes (this can be overriden)
!$OMP SINGLE
  PRINT *, "Hello from Single Section"
!$OMP END SINGLE

  PRINT *,"Hello from thread again"

!$OMP END PARALLEL

  PRINT *. "Hello from outside the parallel section again"

END PROGRAM thread_and_single
