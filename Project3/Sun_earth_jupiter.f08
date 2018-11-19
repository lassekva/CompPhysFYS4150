Program jupiter_change
  use solver_class

  implicit none

  integer,parameter::numbodies=3
  real(8)::Tmax=20,h ! kolisjon ved 2.5445 etter får vi 2.54499
  integer::Num_Steps=10000,m
  real(8),dimension(10)::masses
  real(8),dimension(10,3)::position
  real(8),dimension(10,3)::velocity
  real(8),dimension(numbodies,numbodies,4)::relposition,updaterel
  real(8),dimension(numbodies,3)::relforce
  real(8),dimension(numbodies,3)::updatedforce
  real(8),dimension(numbodies+1)::kinetic,potential,angular

  type(solver)::solar

  solar%mass(1)=1.d0
  solar%mass(2)=3.d-6
  solar%mass(3)=9.5d-4*730

  solar%position(1,1)=3.1647d-3
  solar%position(1,2)=4.4307d-3
  solar%position(1,3)=-1.51447d-4
  solar%velocity(1,1)=365.25*(-3.37957d-6)
  solar%velocity(1,2)=365.25*(6.606862d-6)
  solar%velocity(1,3)=365.25*(7.32297d-8)
  solar%position(2,1)=-9.8825d-1
  solar%position(2,2)=8.49978d-2
  solar%position(2,3)=-1.5199728d-4
  solar%velocity(2,1)=365.25*(-1.68024d-3) ! test 165.25*(-1.68024d-3)
  solar%velocity(2,2)=365.25*(-1.719988d-2)! test 165.25*(-1.719988d-2)
  solar%velocity(2,3)=365.25*(4.34984d-7)! test 165.25*(4.34984d-7)
  solar%position(3,1)=-5.23294d0! orginal-> -5.23294d0, Test with this-> -5.83294d0 OK
  solar%position(3,2)=-1.52515d0
  solar%position(3,3)=1.233648d-1
  solar%velocity(3,1)=365.25*(2.022596d-3)
  solar%velocity(3,2)=365.25*(-6.88771645d-3)
  solar%velocity(3,3)=365.25*(-1.6694179d-5)

  open(2,file="Sun_jupiter_change.dat")
  open(3,file="Earth_jupiter_change.dat")
  open(4,file="Jupiter_change.dat")

  h=Tmax/Num_Steps
  do m=1,Num_Steps

     call relative_position(solar,numbodies,relposition)
     call forces(solar,Numbodies,relposition,relforce)
     call calc_position(solar,numbodies,relforce,h)
     call relative_position(solar,numbodies,updaterel)
     call forces(solar,numbodies,updaterel,updatedforce)
     call calc_velocities(solar,numbodies,relforce,updatedforce,h)
     call kinetic_energy(solar,numbodies,kinetic)
     call potential_energy(solar,numbodies,updaterel,potential)
    ! call angular_momentum(solar,numbodies,updaterel,angular)

!     write(5,*), kinetic(Numbodies+1), potential(numbodies+1)
!     write(7,*), angular(Numbodies+1)

  write(2,*) solar%position(1,1),solar%position(1,2),solar%position(1,3)
  write(3,*) solar%position(2,1),solar%position(2,2),solar%position(2,3)
  write(4,*) solar%position(3,1),solar%position(3,2),solar%position(3,3)

  end do

  close(2)
  close(3)
  close(4)

end program jupiter_change
