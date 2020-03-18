PROGRAM reduce

  USE MPI
  IMPLICIT NONE
  INTEGER :: nproc, rank, rank_red, errcode

  CALL MPI_Init(errcode)

  !Get the total number of processors
  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, errcode)

  !Get the rank of your current processor
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, errcode)

  !MPI_Reduce combines values from all processors. Here it finds the maximum
  !value (MPI_MAX) over all processors. It gets that value on one processor
  !called the "root" processor, here rank 0. The related MPI_Allreduce gives the
  !reduced value to all processors
  CALL MPI_Reduce(rank, rank_red, 1, MPI_INTEGER, MPI_MAX, 0, MPI_COMM_WORLD, &
      errcode)

  IF (rank == 0) PRINT *,'Largest rank is ', rank_red

  !MPI_Allreduce combines values from all processors. Here it finds the sum of
  !the value (MPI_SUM) over all processors. It gets that value on all processors
  CALL MPI_Allreduce(rank, rank_red, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, &
      errcode)

  IF (rank == nproc-1) PRINT *, 'Sum of ranks is ', rank_red

  CALL MPI_Finalize(errcode)

END PROGRAM reduce
