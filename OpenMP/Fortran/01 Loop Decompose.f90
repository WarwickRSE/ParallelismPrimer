PROGRAM loop_decompose

  USE omp_lib
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_its = 10000
  INTEGER :: nproc, i, thread_id
  INTEGER, DIMENSION(:), ALLOCATABLE :: its_per_proc

  nproc = omp_get_max_threads()
  ALLOCATE(its_per_proc(nproc))
  its_per_proc = 0

!$OMP PARALLEL DO
  DO i = 1, max_its
    !This gets a unique ID number for each thread running from 0 -> nproc-1
    !You shouldn't really keep checking this value inside the loop because
    !it won't change but it makes the parallelism a bit simpler
    thread_id = omp_get_thread_num() + 1
    its_per_proc(thread_id) = its_per_proc(thread_id) + 1
  END DO
!$OMP END PARALLEL DO
!After the END PARALLEL DO construct the threads all connect back together
!and there is only one processor after this point

  DO i = 1, nproc
    PRINT '(A, I0, A, I0, A)', 'Processor ', i, ' performed ', &
        its_per_proc(i), ' iterations'
  END DO

  PRINT '(A, I0)','Total work on all processors is ', SUM(its_per_proc)

END PROGRAM loop_decompose
