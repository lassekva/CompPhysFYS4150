import matplotlib.pyplot as plt
import numpy as np
import os
import sys

N = int(sys.argv[1])
#results_b = np.loadtxt('vector%4.4i.dat'%N)
#results_c = np.loadtxt('1vector%4.4i.dat'%N)
u = np.loadtxt('u.dat')
u_ex = np.loadtxt('u_ex.dat')
x = np.linspace(0,1,N+1)
"""
def u(parameter):
    u = 1 - (1 - np.exp(-10))*parameter - np.exp(-10*parameter)
    return u
"""
#plt.plot(x,u(x))
plt.plot(x,u_ex,x,u)
plt.title('Values of vector v with N = %i'%N)
plt.legend(['u(x)_exact', 'u(x)'])
#plt.xlabel('')
#plt.ylabel('')
plt.grid('on')
plt.show()
