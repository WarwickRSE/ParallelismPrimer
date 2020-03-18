PROGRAM collatz

  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

  INTEGER(INT64), PARAMETER :: nums = 400000000, maxits = 1000
  INTEGER, DIMENSION(:), ALLOCATABLE :: counts
  INTEGER(INT64) :: iint, itrial, icurr

  !Allocate arrays for the numbers to test the Collatz conjecture on
  !Counts contains the number of iterations required to reach 1
  ALLOCATE(counts(nums))
  !Set all counts values to -1 as a sentinal. If a value stays -1 it means
  !that there was no convergence
  counts = -1
!$OMP PARALLEL DO PRIVATE(iint, itrial) SHARED(counts) SCHEDULE(dynamic)
  !Loop over all numbers to start the sequence from
  DO iint = 1, nums
    icurr = iint !Capture the value to start from
    !Loop to a maximum number of iterations
    !You can check if any are not converged after maxits by checking
    DO itrial = 1, maxits
      !If icurr is even divide by two
      IF (MOD(icurr,2) == 0) THEN
        icurr = icurr / 2
      ELSE !Otherwise multiply by 3 and add 1
        icurr = icurr * 3 + 1
      END IF
      !If the number reaches 1 then sequence has converged
      IF (icurr == 1) THEN
        counts(iint) = itrial
        EXIT
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO

  PRINT *, 'Number of non-converged items ', COUNT(counts < 0)

END PROGRAM collatz
