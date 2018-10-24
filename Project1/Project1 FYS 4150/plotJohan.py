import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0,1,11)

u = 1-(1-np.exp(-10))*x-np.exp(-10*x)

vector = np.loadtxt('vector0010.dat')
plt.figure()
plt.plot(x,u,label='Exact Solution')
plt.plot(x,vector, label='')
plt.legend()
plt.savefig('vector0010.png')
plt.show()
