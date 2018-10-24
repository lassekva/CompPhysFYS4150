import numpy as np
import matplotlib.pyplot as plt
import os
import sys

h_list = np.zeros(6)
eps = np.zeros(6)

def u(x):
    u = 1-(1-np.exp(-10))*x-np.exp(-10*x)
    return u

for i in range(1,7):
    n=10**i
    h=1./(n+1)
    h_list[i-1]=h
    result=np.loadtxt('res_vector%i.dat'%(i))
    x=np.linspace(h,1-h,n-1)
    eps[i-1]=max(abs((result[1:n]-u(x))/u(x)))

plt.loglog(h_list,eps,'-o')

plt.grid('on')
plt.show()






"""
plt.figure()
plt.plot(x,log10h, 'r--')
#plt.plot(Error,label='Error')
#plt.loglog(log10h,Error)
plt.legend()

#plt.savefig('Error.png')
plt.grid('on')
plt.show()
"""
