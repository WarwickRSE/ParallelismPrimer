PROGRAM collatz

  USE ISO_FORTRAN_ENV
  USE MPI
  IMPLICIT NONE

  INTEGER(INT64), PARAMETER :: nums = 400000000, maxits = 1000
  INTEGER, DIMENSION(:), ALLOCATABLE :: counts
  INTEGER(INT64) :: iint, itrial, icurr, nums_local, non_zero_l, non_zero
  INTEGER :: rank, nproc, errcode, lower

  CALL MPI_INIT(errcode)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)

  nums_local = nums / INT(nproc, INT64)

  !Allocate arrays for the numbers to test the Collatz conjecture on
  !Counts contains the number of iterations required to reach 1
  ALLOCATE(counts(nums_local))
  lower = rank * nums_local
  !Set all counts values to -1 as a sentinal. If a value stays -1 it means
  !that there was no convergence
  counts = -1
  !Loop over all numbers to start the sequence from
  DO iint = 1, nums_local
    icurr = iint + lower !Capture the value to start from
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

  non_zero_l = COUNT(counts < 0)
  CALL MPI_REDUCE(non_zero_l, non_zero, 1, MPI_INTEGER8, MPI_SUM, 0, &
      MPI_COMM_WORLD, errcode)

  IF (rank == 0) PRINT *, 'Number of non-converged items ', non_zero

  CALL MPI_FINALIZE(errcode)

END PROGRAM collatz
