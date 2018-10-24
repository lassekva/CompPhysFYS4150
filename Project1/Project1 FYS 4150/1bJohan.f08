PROGRAM Project1Johan
  IMPLICIT NONE
  character(21) :: filename
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

    ! Equation for f
    DO i=1,n+1
      f(i) = (h**2)*100*exp(-10*(i-1.0)*h) ! Many? 7? flops
    ENDDO

    ! Setting up vectors
    DO i=1,n
      a(i) = -1
      c(i) = -1
      b(i) = 2
    ENDDO

    call cpu_time(start)
! Algorithm
    ! Update b
    DO i=3,n
      b(i) = b(i)-(a(i-1)*c(i-1))/b(i-1) ! 3 flops
    ENDDO

    ! Forward substitution
    DO i=3,n
      f(i) = f(i) - f(i-1)*a(i-1)/b(i-1) ! 3 flops
    ENDDO

    ! Backward substitution
    u(n) = f(n)/b(n) ! 1 flop

    DO i=n-1,2,-1
      u(i) = (f(i)-c(i)*u(i+1))/b(i) ! 3 flops
    ENDDO
! End Algorithm
    call cpu_time(finish)
  !  print '("Time = ",f6.3," seconds.")',finish-start, 'sec'
    print *, f(:)

    10 format(A6,I4.4,A4)
    WRITE(filename,10)'vector',N,'.dat'

    open(unit=1,file=filename)
    write(1,*) u
    close(1)

    ! Write finish message to screen
    WRITE(*,*) 'Solution data written to file'

END PROGRAM Project1Johan
