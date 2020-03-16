PROGRAM counter

  IMPLICIT NONE
  INTEGER :: i, cvar

  cvar = 0

!$OMP PARALLEL DO PRIVATE(i)
  DO i = 1, 1000
  !This is a faster alternative for simple problems to critical sections
  !atomic sections rely on specific computer hardware that allows you to
  !do certain operations (like incrementing a counter) in a single event
  !that can't be interrupted by another thread. The name isn't related to
  !physical atoms but comes from the same Greek root (atomos - indivisible)
!$OMP ATOMIC UPDATE
    cvar = cvar + 1
!$OMP END ATOMIC
  END DO
!$OMP END PARALLEL DO

  PRINT *, cvar

END PROGRAM counter
