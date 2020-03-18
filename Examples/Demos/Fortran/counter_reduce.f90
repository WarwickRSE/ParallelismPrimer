PROGRAM counter

  IMPLICIT NONE
  INTEGER :: i, cvar

  cvar = 0

!This fixes the problem with the counter by using the reduction option
!This says that the specified variable should be local to each thread but
!then combined (reduced) in a specified way. It works, scales well and is 
!probably the best solution where it works but it is quite limited.
!There are a fixed list of reduction operations that can be performed

!$OMP PARALLEL DO PRIVATE(i) REDUCTION(+:cvar)
  DO i = 1, 1000
    cvar = cvar + 1
  END DO
!$OMP END PARALLEL DO

  PRINT *, cvar

END PROGRAM counter
