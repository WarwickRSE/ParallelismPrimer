PROGRAM loop_decompose

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_its = 10000
  INTEGER :: i, its_global

  its_global = 0
!$OMP PARALLEL DO
  DO i = 1, max_its
!This tells the compiler that this code should be atomic
!That means that individual operations are guaranteed to complete
!in a way that can't be interrupted by other threads
!$OMP ATOMIC
    its_global = its_global + 1
!$OMP END ATOMIC
  END DO
!$OMP END PARALLEL DO

PRINT '(A,I0,A)','Counter records ', its_global, ' iterations'

END PROGRAM loop_decompose
