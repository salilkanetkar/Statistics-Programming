import numpy as np
from scipy import linalg
import csv as csv
import pandas as pd
import math
from sklearn import datasets

def eigen_qr(A):
    T = 10000
    A_copy = A.copy()
    r, c = A_copy.shape
    V = np.random.random_sample((r, r))
    for i in range(T):
        Q, _ = myqr_gram_schmidt(V)
        V = np.dot(A_copy, Q)
    Q, R = myqr_gram_schmidt(V)
    return R.diagonal(), Q

def qr(A):
    n, m = A.shape
    R = A.copy()
    Q = np.eye(n)

    for k in range(m-1):
        x = np.zeros((n, 1))
        x[k:, 0] = R[k:, k]
        v = x
        v[k] = x[k] + np.sign(x[k,0]) * np.linalg.norm(x)

        s = np.linalg.norm(v)
        if s != 0:
            u = v / s
            R -= 2 * np.dot(u, np.dot(u.T, R))
            Q -= 2 * np.dot(u, np.dot(u.T, Q))
    Q = Q.T
    return Q, R

def myqr_gram_schmidt(A):
    n,p = A.shape
    Q=np.eye(n)
    R=A.copy()
    U=np.zeros(shape=(n,p))
    U[:,0]=A[:,0]
    #print U
    for k in range(1,p):
        proj=np.zeros(shape=(n,1))
        for i in range(0,k-1):
            a=np.sum(A[:,k]*U[:,i])
            b=np.sum(U[:,i]*U[:,i])
            c=a/b
            ck=c*U[:,i]
            proj=proj+ck
        U[:,k]=A[:,k]-proj[:,0]
    sq=np.zeros(shape=(n,1))
    #print U
    temp=np.zeros(shape=(n,1))
    Q=np.zeros(shape=(n,p))
    for i in range(0,p):
        temp=np.dot(U[:,i],U[:,i])
        sq[i,0]=math.sqrt(np.sum(temp))
        Q[:,i]=U[:,i]/sq[i,0]
    R = np.dot(Q.transpose(),A)
    return Q, R
'''
A = [[12,-51,4],[6,167,-68],[-4,24,-41]]     
A=A.as_matrix()
print A
A=np.dot(A.transpose(),A)
'''
iris1 = datasets.load_iris()
iris = iris1.data[:,:]

#print iris
#iris2=iris.as_matrix(columns=none)
n1,p1=iris.shape
A=np.zeros(shape=(n1,p1))
A=np.dot(iris.transpose(),iris)

D, V = eigen_qr(A)
print D.round(6)
print V.round(6)

# Compare the result with the numpy calculation
eigen_value_gt, eigen_vector_gt = np.linalg.eig(A)
print eigen_value_gt.round(6)
print eigen_vector_gt.round(6)