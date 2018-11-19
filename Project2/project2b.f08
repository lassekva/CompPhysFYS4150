PROGRAM project2b
IMPLICIT NONE
INTEGER :: i, j, N, LDZ, INFO
CHARACTER(1) :: JOBZ
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: Z(:,:), E(:), D(:), WORK(:)
DOUBLE PRECISION :: c, s, t, tau, theta
c = dcos(theta)
s = dsin(theta)
t = s/c
JOBZ = 'V'
WRITE(*,*) 'Enter a value for N: '
READ(*,*) N
LDZ = N

ALLOCATE(E(N-1),Z(N,N),D(N),WORK(max(1,2*N-2)))

DO i=1,N
  E(i)=-1
  D(i)=2
ENDDO


CALL dstev(JOBZ,N,D,E,Z,LDZ,WORK,INFO)

WRITE(*,*) D(1), Z(1,:)
DEALLOCATE(E,Z,D,WORK)
END PROGRAM
