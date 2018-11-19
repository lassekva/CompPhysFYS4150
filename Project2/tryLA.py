import numpy as np
from numpy import linalg as LA

w, v =LA.eig(np.diag((10,2,6)))
d= np.diag((1,2,3,4,5))
print (w)
