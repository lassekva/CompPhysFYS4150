program Project1_b

  implicit NONE

  real :: frac, stop, start
  integer :: N,i,j
  double precision, allocatable, dimension(:) :: a(:),b(:),c(:), v(:),f(:)
  double precision, allocatable :: h
  character(21) :: filename

  write(*,*) 'Enter a value for N:'
  read(*,*) N
  allocate (a(N),b(N),c(N),v(N),f(N))

  h=1./(N+1)

  do i=1,N
    a(i)=-1.
    b(i)=2
    c(i)=-1
    f(i)=(h**2)*(100*exp(-10.*(i-1)*h))
  end do

  a(1)=0
  C(N-1)=0
  !v(1)=0
  !v(N)=0

  call cpu_time(start)


  do i= 2,N
    ! Update b
    b(i)=b(i)-(a(i)*c(i-1))/b(i-1)
    ! Forward substitution
    f(i)=f(i) -(a(i)*f(i))/b(i-1)
  enddo

  V(N-1)=f(N-1)/b(N-1)

  do i=N-2, 2,-1
    v(i)=(f(i)-(c(i)*v(i+1)))/b(i)
  enddo

  call cpu_time(stop)
  write(*,*) 'Time it took to run algorithm:', stop-start, 'sec', f(:)

  10 format(A6,I7.7,A4)
  write(filename,10)'vector',N,'.dat'

  open(unit=1,file=filename)
  write(1,*) v
  close(1)
end program Project1_b
