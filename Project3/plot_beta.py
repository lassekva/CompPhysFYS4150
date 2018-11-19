import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

x_beta_2 = np.loadtxt('xpos_beta_2.dat')
y_beta_2 = np.loadtxt('ypos_beta_2.dat')
x_beta_2_1 = np.loadtxt('xpos_beta_2_1.dat')
y_beta_2_1 = np.loadtxt('ypos_beta_2_1.dat')
x_beta_2_2 = np.loadtxt('xpos_beta_2_2.dat')
y_beta_2_2 = np.loadtxt('ypos_beta_2_2.dat')
x_beta_3 = np.loadtxt('xpos_beta_3.dat')
y_beta_3 = np.loadtxt('ypos_beta_3.dat')


plt.figure()
plt.plot(x_beta_2,y_beta_2, label='2')
plt.plot(x_beta_2_1,y_beta_2_1, label='2.1')
plt.plot(x_beta_2_2,y_beta_2_2, label='2.2')
plt.plot(x_beta_3,y_beta_3, label='3')

plt.legend()
plt.show()
