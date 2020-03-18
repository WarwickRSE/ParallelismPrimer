PROGRAM test

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: nels = 100
  INTEGER, DIMENSION(:), ALLOCATABLE :: el
  INTEGER, DIMENSION(:), ALLOCATABLE :: ct
  CHARACTER(LEN=*), PARAMETER :: symbols &
      = "#*!@ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  INTEGER :: nproc, proc, ix, iproc

  nproc = omp_get_max_threads()
  ALLOCATE(ct(nproc))
  ct = 0
  ALLOCATE(el(nels))

!$OMP PARALLEL PRIVATE(proc, ix)
    proc = omp_get_thread_num() + 1
!Change this SCHEDULE line to see the effects of different schedulers
!$OMP DO SCHEDULE(static)
  DO ix = 1, nels
    ct(proc) = ct(proc) + 1
    el(ix) = proc
  END DO
!$OMP END DO
!$OMP END PARALLEL

 DO iproc = 1, nproc
   PRINT "(A,I0,A,I0,A)", "Thread ", iproc-1 , " handled ", ct(iproc), &
       " elements"
 END DO

 DO ix = 1, nels
   IF (el(ix) > 0 .AND. el(ix) <= LEN(symbols)) THEN
     WRITE(*,'(A)', ADVANCE="NO") symbols(el(ix):el(ix))
   ELSE
     WRITE(*,'(A,I0,A)', ADVANCE="NO") ":",el(ix),":"
   END IF
 END DO

 WRITE(*,'(A)') ""

END PROGRAM test
