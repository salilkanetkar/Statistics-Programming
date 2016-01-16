import numpy as np
A=np.matrix(([1,2,3],[4,5,6],[7,8,9]))
B=np.zeros(shape=(3,3))
B[:,2:3]=A[:,0]


c=A[:,0].*B[:,0]