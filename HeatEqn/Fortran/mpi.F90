MODULE shared_data

  USE mpi
  IMPLICIT NONE

  !Information about the global array
  INTEGER, PARAMETER :: nx = 2000, ny = 2000
  INTEGER, PARAMETER :: tag = 100
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: values

  !Information about the arrays on this processor
  REAL, DIMENSION(:, :), ALLOCATABLE :: values_local, temp_local
  INTEGER :: nx_local, ny_local

  !Pure MPI information
  INTEGER :: nproc, rank
  INTEGER, DIMENSION(2) :: nprocs, coordinates
  INTEGER :: x_min_rank, x_max_rank, y_min_rank, y_max_rank

  CONTAINS

  !Routine the applies interprocessor boundary conditions
  !Leaves true boundaries alone because they have MPI_PROC_NULL
  !for x_min_rank, x_max_rank, y_min_rank, y_max_rank (depending on edge)
  !This makes that part of MPI_Sendrecv a null operation
  SUBROUTINE bcs(array)

    REAL, DIMENSION(0:, 0:), INTENT(INOUT) :: array
    INTEGER :: ierr

    !Send left most strip of cells left and receive into right guard cells
    CALL MPI_Sendrecv(array(1,:), ny_local+2, MPI_REAL, x_min_rank, &
        tag, array(nx_local+1,:), ny_local+2, MPI_REAL, x_max_rank, &
        tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)

    !Send right most strip of cells right and receive into left guard cells
    CALL MPI_Sendrecv(array(nx_local, :), ny_local+2, MPI_REAL, &
        x_max_rank, tag, array(0,:), ny_local+2, MPI_REAL, x_min_rank, &
        tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)

    !Now equivalently in y
    CALL MPI_Sendrecv(array(:,1), nx_local+2, MPI_REAL, y_min_rank, &
        tag, array(:,ny_local+1), nx_local+2, MPI_REAL, y_max_rank, &
        tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)

    CALL MPI_Sendrecv(array(:,ny_local), nx_local+2, MPI_REAL, &
        y_max_rank, tag, array(:,0), nx_local+2, MPI_REAL, y_min_rank, &
        tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)

    !Use MPI_PROC_NULL to test if you are on a real boundary
    !If you are then apply a real boundary condition
    IF (x_min_rank == MPI_PROC_NULL) array(0,:) = 1.0
    IF (x_max_rank == MPI_PROC_NULL) array(nx_local+1,:) = 10.0
    IF (y_min_rank == MPI_PROC_NULL) array(:,0) = 1.0
    IF (y_max_rank == MPI_PROC_NULL) array(:, ny_local+1) = 10.0

  END SUBROUTINE bcs



  !Routine to set up the MPI system
  SUBROUTINE setup_mpi

    INTEGER :: ierr, ix, iy

    CALL MPI_Init(ierr)

    CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    !How to subdivide your domain over processors is a rather vexed question
    !If you have n processors and a 2D problem then you want to work out how to
    !split it up into (l, m) processor array. 
    IF (nproc /= 1 .AND. nproc /=2 .AND. nproc /=4 .AND. nproc /= 8 .AND. &
        nproc /= 16) THEN

      IF (rank == 0) THEN
        PRINT *,'Demo code only works on 1, 2, 4, 8 or 16 processors'
        CALL MPI_Abort(MPI_COMM_WORLD, 2, ierr)
      END IF
    END IF

    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
    CALL MPI_Dims_create(nproc, 2, nprocs, ierr)

#ifndef NODISPLAY
    IF (rank == 0) THEN
      PRINT *,'Processor decomposition is ', nprocs
    ENDIF
#endif
    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

    !Divide the global size (nx x ny) per processor
    nx_local = nx / nprocs(1)
    ny_local = ny / nprocs(2)

    !Calculate your co-ordinates in a Cartesian grid
    coordinates(2) = rank/nprocs(1)
    coordinates(1) = rank - coordinates(2) * nprocs(1)

    !Calculate which rank is along each edge of your domain
    !NOTE at this stage you have not dealt with processors at the edge
    !of your logical processor decomposition
    x_max_rank = rank + 1
    x_min_rank = rank - 1
    y_max_rank = rank + nprocs(1)
    y_min_rank = rank - nprocs(1)

    !If this processor is at an edge then some of the neighbours
    !will be MPI_PROC_NULL
    IF (coordinates(1) == 0) x_min_rank = MPI_PROC_NULL
    IF (coordinates(1) == nprocs(1)-1) x_max_rank = MPI_PROC_NULL
    IF (coordinates(2) == 0) y_min_rank = MPI_PROC_NULL
    IF (coordinates(2) == nprocs(2)-1) y_max_rank = MPI_PROC_NULL 

  END SUBROUTINE setup_mpi

END MODULE shared_data

PROGRAM parallel

  USE shared_data

  IMPLICIT NONE

  INTEGER :: ix, iy, icycle, ierr
  REAL :: sum_local, sum_global

  CALL setup_mpi

  ALLOCATE(values_local(0:nx_local+1, 0:ny_local+1))
  ALLOCATE(temp_local(0:nx_local+1, 0:ny_local+1))
  values_local = 5.5

  !Now iterate
  DO icycle = 1, 5000
    CALL bcs(values_local)
    !Operate on the local variables just the same as the global ones in serial
    DO iy = 1, ny_local
      DO ix = 1, nx_local
        temp_local(ix,iy) = 0.25 * (values_local(ix+1,iy) + &
             values_local(ix,iy+1) + values_local(ix-1,iy) + &
             values_local(ix,iy-1))
      END DO
    END DO
    values_local = temp_local
  END DO

  sum_local = SUM(values_local(1:nx_local,1:ny_local))
  CALL MPI_Reduce(sum_local, sum_global, 1, MPI_REAL, MPI_SUM, 0, &
      MPI_COMM_WORLD, ierr)

  IF (rank == 0) PRINT *,sum_global/REAL(nx*ny)

  CALL MPI_Finalize(ierr)

END PROGRAM parallel
