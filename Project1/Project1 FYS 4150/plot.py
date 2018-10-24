import matplotlib.pyplot as plt
import numpy as np
import os
import sys

N = int(sys.argv[1])
results_b = np.loadtxt('vector%4.4i.dat'%N)
results_c = np.loadtxt('1vector%4.4i.dat'%N)

x = np.linspace(0,1,N+1)

def u(parameter):
    u = 1 - (1 - np.exp(-10))*parameter - np.exp(-10*parameter)
    return u


if N<11:
    plt.plot(x,u(x))
    plt.plot(x,results_b,'r-')
    plt.plot(x,results_c,'g-.',alpha=0.8)
else:
    plt.plot(x,u(x), alpha=0.4)
    plt.plot(x,results_b,'r:')
    plt.plot(x,results_c,'g-.',alpha=0.5)
plt.suptitle('Solutions from Closed form, General algorithm and Simplified algorithm')
plt.title('Values of vector v with N = %i'%N)
plt.legend(['Closed form solution','General algorithm solution','Simplified algorithm'])
plt.xlabel('x values', fontsize=14)
plt.ylabel('u(x)', rotation=0 ,fontsize=14)
plt.grid('on')
plt.show()
