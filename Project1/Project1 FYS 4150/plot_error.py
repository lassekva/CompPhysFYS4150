import numpy as np
import matplotlib.pyplot as plt

#x = np.linspace(0,1,11)
#x = np.linspace(0,1,101)
#x = np.linspace(0,1,1001)
log10h = 1/(np.linspace(0,10000,9996)+1)
# u = 1-(1-np.exp(-10))*x-np.exp(-10*x)

Error = np.loadtxt('Error.dat')



plt.figure()
#plt.plot(Error,label='Error')
plt.loglog(log10h,Error)
plt.legend()
plt.savefig('Error.png')
plt.show()
