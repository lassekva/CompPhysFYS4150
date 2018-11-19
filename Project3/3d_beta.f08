PROGRAM change_beta

  CALL verlet()

END PROGRAM change_beta


SUBROUTINE verlet()
  IMPLICIT NONE
  DOUBLE PRECISION,ALLOCATABLE :: x(:),y(:),vx(:),vy(:),r(:),time(:)
  INTEGER :: n,i ! Number of planets
  DOUBLE PRECISION :: solar_mass, earth_mass,pi,FourPi2,FinalTime,h,solar_mass_value
  CHARACTER(20) ::filename
  REAL :: beta
  REAL :: start2,finish2

  ! Variables
  FinalTime = 100.0
  n = 100000
  h = FinalTime/REAL(n)
  pi = 4.D0*DATAN(1.D0)
  FourPi2 = 4*pi*pi
  solar_mass_value = 1.989e30
  solar_mass = 1 ! 1 AU
  earth_mass = 6e24/solar_mass_value ! earth mass in units of AU

  ALLOCATE(x(n+2),y(n+2),vx(n+2),vy(n+2),r(n+2),time(n+2))

  ! Initial conditions
  beta=4
  time(1) = 0.0
  x(1) = 1.0
  y(1) = 0.0
  vx(1) = 0.0
  vy(1) = 2.0*pi
  r(1) = sqrt((x(1)** 2)+(y(1)**2))

  call cpu_time(start2)

  DO i=1,n+1
      time(i+1) = time(i) + h
      x(i+1) = x(i) + h*vx(i) - ((h**2)/2)*(FourPi2)*x(i)/(r(i)**beta)
      y(i+1) = y(i) + h*vy(i) - ((h**2)/2)*(FourPi2)*y(i)/(r(i)**beta)
      r(i+1) = sqrt((x(i+1)**2)+(y(i+1)**2))
      vx(i+1) = vx(i) - (h/2)*((FourPi2*x(i+1))/(r(i+1)**2)+(FourPi2*x(i))/(r(i)**beta))
      vy(i+1) = vy(i) - (h/2)*((FourPi2*y(i+1))/(r(i+1)**2)+(FourPi2*y(i))/(r(i)**beta))
      !WRITE (*,*) time(i)
  ENDDO
  call cpu_time(finish2)
  print '("Time for Varlet  = ",f6.3," seconds.")',finish2-start2

  10 format(A15)
  WRITE(filename,10)'xpos_beta_3.dat'
  OPEN(unit=1,file=filename)
  WRITE(1,*) x
  CLOSE(1)

  11 format(A15)
  WRITE(filename,11)'ypos_beta_3.dat'
  OPEN(unit=1,file=filename)
  WRITE(1,*) y
  CLOSE(1)


  DEALLOCATE(x,y,vx,vy,r,time)

END SUBROUTINE verlet
