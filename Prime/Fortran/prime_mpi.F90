
MODULE prime_finder
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

  INTEGER(INT64), PARAMETER ::  small_primes_len = 20
  INTEGER(INT64), DIMENSION(small_primes_len), PARAMETER :: &
      small_primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, &
      53, 59, 61, 67, 71]
  INTEGER, PARAMETER :: max_small_prime = 71

  CONTAINS

  FUNCTION check_prime(num)
    INTEGER(INT64), INTENT(IN) :: num
    LOGICAL :: check_prime

    INTEGER(INT64) :: index, end

    end = CEILING(SQRT(REAL(num, REAL64)))

    check_prime = .FALSE.
    !First check against the small primes
    DO index = 1, small_primes_len
      IF (small_primes(index) == num) THEN
        check_prime = .TRUE.
        RETURN
      END IF
      IF (MOD(num, small_primes(index)) == 0) THEN
        RETURN
      END IF
    END DO

    !Test higher numbers, skipping all the evens
    DO index = max_small_prime + 2, end, 2
      IF (MOD(num, index) == 0) RETURN
    END DO
    check_prime = .TRUE.
  END FUNCTION check_prime

END MODULE prime_finder

PROGRAM primes

  USE prime_finder
  USE mpi
  IMPLICIT NONE
  INTEGER(INT64), PARAMETER :: maxval = 20000001_INT64
  INTEGER(INT64) :: ct, i, nums_per_proc, local_min, local_max, ct_global
  INTEGER :: rank, nproc, errcode

  CALL MPI_INIT(errcode)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)

  nums_per_proc = (maxval-1) / INT(nproc, INT64)
  local_min = 2 + INT(rank, INT64) * nums_per_proc
  local_max = 1 + INT(rank + 1, INT64) * nums_per_proc

  ct = 0_INT64
  DO i = local_min, local_max
    IF (check_prime(i)) ct = ct + 1
  END DO

  CALL MPI_REDUCE(ct, ct_global, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, &
      errcode)
  IF (rank == 0) PRINT *, "Number of primes = ", ct_global

  CALL MPI_FINALIZE(errcode)

END PROGRAM primes
