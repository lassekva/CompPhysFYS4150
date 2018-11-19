import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from mpl_toolkits.mplot3d import Axes3D

Sun_escape = pd.read_csv('Sun_2.dat',sep='\s+',header=None,names=['x','y','z'])
Planet_escape_88 = pd.read_csv('Planet.dat',sep='\s+',header=None,names=['x','y','z'])


pax_88=Planet_escape_88['x'][Planet_escape_88.index[-1]]
pay_88=Planet_escape_88['y'][Planet_escape_88.index[-1]]




sux=Sun_escape['x'][Sun_escape.index[-1]]
suy=Sun_escape['y'][Sun_escape.index[-1]]



plt.figure(figsize=(15,10))
plt.subplot(121)
plt.plot(Sun_escape['x'],Sun_escape['y'],'yo',label='Sun')#,Sun_escape['z']
plt.plot(Planet_escape_88['x'],Planet_escape_88['y'],label=r'v=8.88 AU/year')#,Planet_escape['z']
plt.plot([pax_88],[pay_88],'bo', label=r'Earths position')
plt.xlabel('x [AU]',size=14)
plt.ylabel('y [AU]',size=14)
plt.legend()
plt.grid(True)


plt.legend()
plt.grid(True)
plt.show()
