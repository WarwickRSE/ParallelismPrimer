PROGRAM main
  USE MPI
  IMPLICIT NONE
  INTEGER :: errcode
  INTEGER :: rank, nproc
  CALL MPI_Init(errcode)
  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, errcode)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, errcode)
  PRINT '(A, I3, A, I3)', 'I am processor ', rank + 1, ' of ', nproc
  CALL MPI_Finalize(errcode)
END PROGRAM main
