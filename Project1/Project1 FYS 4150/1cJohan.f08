PROGRAM Project1Johan
  IMPLICIT NONE
  character(22) :: filename
  INTEGER :: n ! User input number
  INTEGER :: i
  REAL :: h
  REAL, ALLOCATABLE, DIMENSION(:) :: b(:),u(:),f(:),a(:),c(:) ! a and c arrays are the same
  REAL :: start,finish

  ! Get n from user
    WRITE(*,*) 'Input a number: '
    READ(*,*) n

    ALLOCATE (a(n+1),b(n+1),f(n+1),u(n+1),c(n+1) ) ! Initializing vectors


    h = 1.0/n
    b(1)=2; b(n+1)=2; u(1)=0; u(n+1)=0
    ! Expression for f
    DO i=1,n+1
      f(i) = (h**2)*100*exp(-10*(i-1.0)*h) ! Many flops
    ENDDO
    call cpu_time(start)
! Algorithm
    ! Updating b
    DO i=2,n
      b(i) = (i)/(i-1.0) ! 2 flops
    ENDDO

    ! Forward substitution
    DO i=3,n
      f(i) = f(i) + f(i-1)/b(i-1) ! 2 flops
    ENDDO

    ! Backward substitution
    u(n) = f(n)/b(n)

    DO i=n-1,2,-1
      u(i) = (f(i)+u(i+1))/b(i) ! 2 flops
    ENDDO
! End Algorithm
    call cpu_time(finish)
    print '("Time = ",f6.3," seconds.")',finish-start

    10 format(A7,I4.4,A4)
    WRITE(filename,10)'vector',N,'.dat'

    open(unit=1,file=filename)
    write(1,*) u
    close(1)

    ! Write finish message to screen
    WRITE(*,*) 'Solution data written to file'

END PROGRAM Project1Johan
