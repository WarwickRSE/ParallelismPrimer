PROGRAM loop_decompose

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_its = 10000
  INTEGER :: i, its_global


its_global = 0
!$OMP PARALLEL DO
  DO i = 1, max_its
!Only one processor at a time can be in the critical section
!All the others have to queue up to enter this
!If you code spends long in critical sections then it will be barely
!and faster than if it is running on a single processor
!$OMP CRITICAL
    its_global = its_global + 1
!$OMP END CRITICAL
  END DO
!$OMP END PARALLEL DO

PRINT '(A,I0,A)','Counter records ', its_global, ' iterations'

END PROGRAM loop_decompose
