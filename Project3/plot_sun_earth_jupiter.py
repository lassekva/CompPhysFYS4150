import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from mpl_toolkits.mplot3d import Axes3D

sun = pd.read_csv('Sun_jupiter_change.dat',sep='\s+',header=None,names=['x','y','z'])
earth = pd.read_csv('Earth_jupiter_change.dat',sep='\s+',header=None,names=['x','y','z'])
jupiter = pd.read_csv('Jupiter_change.dat',sep='\s+',header=None,names=['x','y','z'])

# Get last elementsself.
jux=jupiter['x'][jupiter.index[-1]]
juy=jupiter['y'][jupiter.index[-1]]
juz=jupiter['z'][jupiter.index[-1]]

eax=earth['x'][earth.index[-1]]
eay=earth['y'][earth.index[-1]]
eaz=earth['z'][earth.index[-1]]

sux=sun['x'][sun.index[-1]]
suy=sun['y'][sun.index[-1]]
suz=sun['z'][sun.index[-1]]


fig=plt.figure(figsize=(8,8))
ax = fig.add_subplot(111, projection='3d')
plt.plot(sun['x'],sun['y'],sun['z'],'yo',label='Sun')
plt.plot(earth['x'],earth['y'],earth['z'],label='Earth')
plt.plot(jupiter['x'],jupiter['y'],jupiter['z'], label='Jupiter ')
plt.plot([jux],[juy],[juz],'ro', label='Jupiters position')
plt.plot([eax],[eay],[eaz],'bo', label='Earths position')
plt.plot([sux],[suy],[suz],'go', label='Sun position')
ax.set_xlabel('x [AU]',size=14)
ax.set_ylabel('y [AU]',size=14)
ax.set_zlabel('z [AU]',size=14)
#ax.set_zlim(-2,2)
#plt.savefig('20_sun_earth_jupiter730.png')
ax.legend()
ax.grid(True)
plt.show()
#print(juz)
