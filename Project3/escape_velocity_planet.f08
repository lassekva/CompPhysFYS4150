Program Sun_escape
  use solver_class

  implicit none

  integer,parameter::numbodies=2
  real(8)::Tmax=1000,h !250
  integer::Num_Steps=1000000,m !10000
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
  !solar%mass(3)=3.d-6

  solar%position(1,1)=0
  solar%position(1,2)=0
  solar%position(1,3)=0
  solar%velocity(1,1)=0
  solar%velocity(1,2)=0
  solar%velocity(1,3)=0
  solar%position(2,1)=1
  solar%position(2,2)=0
  solar%position(2,3)=0
  solar%velocity(2,1)=0
  solar%velocity(2,2)=8.88!8.876184143093095
  solar%velocity(2,3)=0
  ! solar%position(3,1)=1
  ! solar%position(3,2)=0
  ! solar%position(3,3)=0
  ! solar%velocity(3,1)=0
  ! solar%velocity(3,2)=2*3.14
  ! solar%velocity(3,3)=0


  open(2,file="Sun_escape.dat")
  open(3,file="Planet_escape_88.dat")
  !open(4,file="Planet.dat")


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

   write(2,*) solar%position(1,1),solar%position(1,2),solar%position(1,3)
   write(3,*) solar%position(2,1),solar%position(2,2),solar%position(2,3)
   !write(4,*) solar%position(3,1),solar%position(3,2),solar%position(3,3)
 end do

  close(2)
  close(3)
  !close(4)

end program Sun_escape
