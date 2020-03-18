PROGRAM loop_decompose

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_its = 10000
  INTEGER :: i, its_global


its_global = 0
!$OMP PARALLEL DO
  DO i = 1, max_its
    its_global = its_global + 1
  END DO
!$OMP END PARALLEL DO

!This will give the wrong counter result
!This is because adding to the counter involves three operations
!1) Get the current value
!2) Increment it
!3) Put it back
!In serial you don't worry about this, but in parallel what happens
!if two processors get the current value at the same time? They both increment
!it separately and put back a value which is 1 greater than it was before
!This means that this counter will be
!1) Lower than the true value
!2) Random because the error only occurs when two thread happen to try to
!increment the counter at the same time
PRINT '(A,I0,A)','Counter records ', its_global, ' iterations'

END PROGRAM loop_decompose
