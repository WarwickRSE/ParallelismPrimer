PROGRAM counter

  IMPLICIT NONE
  INTEGER :: i, cvar

  cvar = 0

!$OMP PARALLEL DO PRIVATE(i)
  DO i = 1, 1000
    !This starts a critical section. Only one thread at a time can be 
    !in a critical section but they can enter in any order
!$OMP CRITICAL
    cvar = cvar + 1
!$OMP END CRITICAL
  END DO
!$OMP END PARALLEL DO

  PRINT *, cvar

END PROGRAM counter
