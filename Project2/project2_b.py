import numpy as np
from numpy import linalg as LA
import sys

N=int(input('Value for N:'))
#print('N is: %d'%N)

h=1/N
d=2./h**2
a=-1./h**2
A=np.zeros((N,N))

A[0,0]=d
A[0,1]=a



for i in range(1,N+1):
    A[i,i]=d
    A[i,i-1]=a
    if i==N-1:
        break
    A[i,i+1]=a

eigval=LA.eigvals(A)

eigenvalues, eigenvector = LA.eig(A)

permute = eigenvalues.argsort()
Eigenvalues = eigenvalues[permute]
Eigenvector = eigenvector[:,permute]

#Analytic eigenvalues
lam=np.zeros(N+1)
for j in range(1,N+1):
    lam[j]=d+2*a*np.cos((j*np.pi)/(N+1))

lamb=lam[1:]


#print
print('Diagonal Matrix: \n',A)

print('Analytic eigenvalues: \n',lamb)



#print('Eigenvalues: \n',eigval)
#print('Eigenvalue: \n' ,eigenvalues)
#print('Eigenvectors: \n', eigenvector)

print('Eigenvalues: \n',Eigenvalues)
print('Eigenvectors: \n', Eigenvector)


# Diagonalize
#D=np.zeros(N)

D=np.diag(A)
print('Diagonal Matrix \n', D)
