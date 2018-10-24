program project1_1c
implicit NONE

real :: frac, stop, start
integer :: N, i, j
double precision, allocatable, dimension(:) :: a(:),b(:),c(:),v(:),b_(:)
double precision, allocatable :: h
character(22) :: filename
write (*,*) 'Enter value for N: '
read (*,*) N
allocate (a(N),b(N),c(N),v(N),b_(N))

h = 1./(N+1)

do i = 1, N
  b(i) = (real(i-1)+1.)/(real(i-1))
  b_(i) = (h**2)*(100*exp(-10.*(i-2)*h))
end do
b(1) = 2.
v(1) = 0
v(N) = 0

call cpu_time(start)

do i = 2, N
  !b(i) = b(i) - 1./b(i-1)
  !b_(i) = b_(i) + ((b_(i-1)/b(i-1))) ! 2n flops
  b_(i) = b_(i) + (b_(i-1)*(((i-1)+1)/(i-1)))
end do

v(N-1) = b_(N-1)/b(N-1)

do i = N-1, 3, -1
  !v(i) = (b_(i) + (v(i+1)/b(i))) ! 2n flops
  v(i-1) = ((i-1)*(b_(i-1)+v(i)))/i
end do

call cpu_time(stop)

write(*,*) 'Time to run the algorithm:', stop-start, 'sec', b_(:)

10 format(A7,I4.4,A4)
write(filename,10)'1vektor',N,'.dat'

open(unit=1,file=filename)
write(1,*) v
close(1)

end program
