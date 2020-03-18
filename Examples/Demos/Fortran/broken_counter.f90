PROGRAM counter

  IMPLICIT NONE
  INTEGER :: i, cvar

  cvar = 0

!This example should just tot up the number of times that the loop is executed
!but it doesn't work properly. This is because adding 1 to cvar is actually a 3
!part process. You get the existing value of cvar, add one to it and then put it
!back. On a single processor you don't have to worry about it but with multiple
!processors you can have a problem. If 2 processors get the current value at the
!same time, then they will both independently add 1 to that value and put it
!back. This leads to the value only being incremented by 1 even though there
!have been two adds. There are a variety of options for fixing this but 
!the classical one is a "critical section" that only allows a single thread
!in at a time

!$OMP PARALLEL DO PRIVATE(i)
  DO i = 1, 1000
    cvar = cvar + 1
  END DO
!$OMP END PARALLEL DO

  PRINT *, cvar

END PROGRAM counter
