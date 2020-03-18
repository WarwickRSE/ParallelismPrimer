MODULE shared_data

  IMPLICIT NONE

  !Information about the global array
  INTEGER, PARAMETER :: nx = 2000, ny = 2000
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: values, temp

  CONTAINS

  !Routine to fill ghost cells with boundary values
  !In this case those are constants, but that is not
  !in general true for all boundary conditions
  !So need to have the function
  SUBROUTINE bcs(array)

    REAL, DIMENSION(0:, 0:), INTENT(INOUT) :: array

    array(0,:) = 1.0
    array(nx+1,:) = 10.0
    array(:,0) = 1.0
    array(:, ny+1) = 10.0

  END SUBROUTINE bcs

END MODULE shared_data

PROGRAM serial

  USE shared_data

  IMPLICIT NONE

  INTEGER :: ix, iy, icycle

  !Set initial default value
  values = 5.5
  !Apply boundary conditions
  CALL bcs(values)

  !Now iterate (in reality you would test for convergence rather than
  !a fixed number of iterations
  DO icycle = 1, 5000
    !Operate on the data
    DO iy = 1, ny
      DO ix = 1, nx
        temp(ix,iy) = 0.25 * (values(ix+1,iy) + &
             values(ix,iy+1) + values(ix-1,iy) + &
             values(ix,iy-1))
      END DO
    END DO
    values(1:nx,1:ny) = temp(1:nx,1:ny)

    !Now apply the boundary conditions
    CALL bcs(values)
  END DO

  PRINT *, SUM(values(1:nx,1:ny))/ REAL(nx*ny)

END PROGRAM serial
