
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
  IMPLICIT NONE
  INTEGER(INT64) :: ct, i

  ct = 0_INT64
!$OMP PARALLEL DO PRIVATE(i) REDUCTION(+:ct) SCHEDULE(guided)
  DO i = 2_INT64, 20000000_INT64
    IF (check_prime(i)) ct = ct + 1
  END DO
!$OMP END PARALLEL DO

  PRINT *, "Number of primes = ", ct

END PROGRAM primes
