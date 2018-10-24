PROGRAM Project1
  IMPLICIT NONE
  character(21) :: filename
  INTEGER :: n ! User input number
  INTEGER :: i,k
  REAL :: h
  REAL, ALLOCATABLE, DIMENSION(:) :: b(:),u(:),f(:),a(:),c(:),u_exact(:) ! a and c arrays are the same
  REAL, ALLOCATABLE, DIMENSION(:) :: Rel_error(:), u_error(:)

! Get n from user
  WRITE(*,*) 'Input a number: '
  READ(*,*) n

  ALLOCATE (u_error(n-4))

  DO k=5,n
      n = k
      ALLOCATE (a(n+1),b(n+1),f(n+1),u(n+1),c(n+1),u_exact(n+1),Rel_error(n+1)) ! Initializing vectors

      h = (1.0/n)
      b(1)=2; b(n+1)=2; u(1)=0; u(n+1)=0
      ! Expression for f
      DO i=1,n+1
        f(i) = (h**2)*100*exp(-10*(i-1.0)*h)
        u_exact(i) = 1-(1-exp(-10.0))*((i-1.0)*h)-exp(-10*(i-1.0)*h)
      ENDDO

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

      ! Relative error
      DO i=2,n
        Rel_error(i) = (abs((u_exact(i)-u(i))/u_exact(i)))
      ENDDO

      u_error(k-4) = (maxval(Rel_error, dim=1, mask=(Rel_error>0)))

      DEALLOCATE (a)
      DEALLOCATE (b)
      DEALLOCATE (c)
      DEALLOCATE (f)
      DEALLOCATE (u)
      DEALLOCATE (u_exact)
      DEALLOCATE (Rel_error)

      ! Write finish message to screen
      WRITE(*,*) k
  ENDDO

  12 format(A5,A4)
  WRITE(filename,12)'Error','.dat'
  open(unit=1,file=filename)
  write(1,*) u_error
  close(1)


END PROGRAM Project1
