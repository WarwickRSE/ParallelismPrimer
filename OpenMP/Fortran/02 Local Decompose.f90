PROGRAM loop_decompose

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_its = 10000
  INTEGER :: nproc, i, thread_id, its


!This splits the previous OMP PARALLEL DO directive into two
!And OMP PARALLEL section that splits the code up into separate threads
!and an OMP DO command that automatically decomposes a loop over processors
!There is also a PRIVATE directive here that informs the compiler that the
!"its" variable is private (i.e. different) in each thread
!$OMP PARALLEL PRIVATE(its)
  its = 0
!$OMP DO
  DO i = 1, max_its
    its = its + 1
  END DO
!$OMP END DO

!This print now has to be in the PARALLEL section or you won't get the
!information from each processor
 PRINT '(A, I0, A, I0, A)', 'Processor ', omp_get_thread_num(), ' performed ', &
        its, ' iterations'
!$OMP END PARALLEL

END PROGRAM loop_decompose
