'''
Name: Salil Kanetkar
UCLA ID: 704557096
As a part of this program, the data set has been modified.
The header rows were removed and the states column was removed.
After importing the data set, it has been normalized.
'''
import numpy as np
from scipy import linalg
import csv as csv
import pandas as pd

def mySweep(B, r):
    A = np.copy(B)
    n, c = A.shape
    for k in range(r):
        for i in range(n):
            for j in range(n):
                if i != k and j != k:
                    A[i,j] = A[i,j]- A[i,k]*A[k,j]/A[k,k]
        for i in range(n):
            if i != k:
                A[i,k] = A[i,k]/A[k,k]
        for j in range(n):
            if j != k:
                A[k,j] = A[k,j]/A[k,k]
        A[k,k] = -1/A[k,k]     
    return A
    
def qr(A):
    A1 = A.copy()
    A = A1.as_matrix(columns=None)
    n, m = A.shape
    R = A.copy()
    Q = np.eye(n)
    for k in range(m-1):
        x = np.zeros((n, 1))
        nx, cx = X.shape
        x[k:nx, 0] = R[k:nx, k]
        v = x
        v[k] = x[k] + np.sign(x[k,0]) * np.linalg.norm(x)

        s = np.linalg.norm(v)
        if s != 0:
            u = v / s
            R -= 2 * np.dot(u, np.dot(u.T, R))
            Q -= 2 * np.dot(u, np.dot(u.T, Q))
    Q = Q.T
    return Q, R

def solve1(R1,Y1):
    n,p = R1.shape
    R1 = R1.as_matrix(columns=None)
    Y1 = Y1.as_matrix(columns=None)
    Z1=np.concatenate((R1[0:p,0:p],Y1),axis=1)
    intr = np.ones(shape=(1,1))
    Z2=np.concatenate([Y1.transpose(),intr],axis=1)
    Z=np.concatenate((Z1,Z2),axis=0)
    X=mySweep(Z,p)
    res=X[0:p,p]
    return(res)
    
def mylm_qr(X,Y):
    n2,p=X.shape
    one=np.ones(shape=(n2,1))
    intercept = pd.DataFrame(one,columns=['one'])
    X1=pd.DataFrame(X)
    Y1=pd.DataFrame(Y)
    Z=pd.concat([intercept,X1,Y1],axis=1)
    _, R = qr(Z)
    R1 = R[0:p+1, 0:p+1]
    Y1 = R[0:p+1, p+1]
    R1 = pd.DataFrame(R1)
    Y1 = pd.DataFrame(Y1)
    beta = solve1(R1, Y1)
    return beta   
    
def mylm_sweep(X,Y):
    n2,p=X.shape
    one=np.ones(shape=(n2,1))
    intercept = pd.DataFrame(one,columns=['one'])
    X1=pd.DataFrame(X)
    Y1=pd.DataFrame(Y)
    z=pd.concat([intercept,X1,Y1],axis=1)
    z1=z.transpose()
    A=np.dot(z1,z)
    S=mySweep(A,p+1)
    beta_hat=S[0:p+1,p+1]
    return(beta_hat)    
    
mat=np.array(list(csv.reader(open("state_x771.csv","rb"),delimiter=','))).astype('float')
n1 = mat.shape[0]
m = mat.shape[1]
col_sd=mat.std(axis=0)
col_mean=mat.mean(axis=0)
for i in range(n1):
    for k in range(m):
        mat[i,k]=(mat[i,k]-col_mean[k])/col_sd[k]
X = mat[:,[0,1,2,4,5,6,7,8]]
Y = mat[:,[3]]

beta = mylm_qr(X,Y)
beta_hat = mylm_sweep(X,Y)
print beta
print beta_hat