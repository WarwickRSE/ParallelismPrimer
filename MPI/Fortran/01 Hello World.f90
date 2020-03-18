PROGRAM main
  USE MPI
  IMPLICIT NONE
  INTEGER :: errcode
  CALL MPI_Init(errcode)
  PRINT *, "Multiprocessor code"
  CALL MPI_Finalize(errcode)
END PROGRAM main
