program project1_1b
implicit none

real :: frac, stop, start
integer :: N, i, j
double precision, allocatable, dimension(:) :: a(:),b(:),c(:),v(:),b_(:)
double precision, allocatable :: h
character(21) :: filename
write (*,*) 'Enter value for N: '
read (*,*) N
allocate (a(N),b(N),c(N),v(N),b_(N))

h = 1./(N+1)

do i = 1, N
  a(i) = -1.
  b(i) = 2.
  c(i) = -1.
  b_(i) = (h**2)*(100*exp(-10.*(i-1)*h))
end do

a(1) = 0
c(N) = 0
v(1) = 0
v(N) = 0

call cpu_time(start)

do i = 2, N
  frac = a(i-1)/b(i-1) ! 1n flop
  b(i) = b(i) - (frac*c(i-1)) ! 2n flops
  b_(i) = b_(i) - (frac*b_(i-1)) ! 2n flops
end do

v(N-1) = b_(N-1)/b(N-1)

do i = N-2, 2, -1
  v(i) = (b_(i) - (c(i)*v(i+1)))/b(i) ! 3n flops
end do
! 8n flops in total
call cpu_time(stop)
write(*,*) 'Time to run the algorithm:', stop-start, 'sec', b_(:)

10 format(A6,I4.4,A4)
write(filename,10)'vektor',N,'.dat'

open(unit=1,file=filename)
write(1,*) v
close(1)

end program
