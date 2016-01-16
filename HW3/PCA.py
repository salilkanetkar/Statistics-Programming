'''
Name: Salil Kanetkar
UCLA ID: 704557096
QR function has been written using the Gram Schmidt Method.
'''
import numpy as np
import math
from sklearn import datasets

def my_eigen(A):
    T = 1000
    A_copy = A.copy()
    r, c = A_copy.shape
    V = np.random.random_sample((r, r))
    for i in range(T):
        Q, _ = myqr_gram_schmidt(V)
        V = np.dot(A_copy, Q)
    Q, R = myqr_gram_schmidt(V)
    return R.diagonal(), Q
 
def myqr_gram_schmidt(A):
    n,p = A.shape
    Q=np.eye(n)
    R=A.copy()
    U=np.zeros(shape=(n,p))
    U[:,0]=A[:,0]
    for k in range(1,p+1):
        proj=np.zeros(shape=(n,1))
        for i in range(0,k-1):
            f=i+1
            tempa=A[:,k-1]
            tempu=U[:,i]
            tempm=np.zeros(shape=(n,1))
            for w in range(0,n):        
                tempm[w]=tempa[w]*tempu[w]
            a=np.sum(tempm)
            b=U[:,i]*U[:,i]
            b=np.sum(U[:,i]*U[:,i])
            c=a/b
            ck=c*U[:,i:f]
            proj=proj+ck
        U[:,k-1:k]=A[:,k-1:k]-proj[:,:1]
    sq=np.zeros(shape=(n,1))
    temp=np.zeros(shape=(n,1))
    Q=np.zeros(shape=(n,p))
    for i in range(0,p):
        temp=np.dot(U[:,i],U[:,i])
        sq[i,0]=math.sqrt(np.sum(temp))
        Q[:,i]=U[:,i]/sq[i,0]
    R = np.dot(Q.transpose(),A)
    return Q, R

def my_pca(A):
    n1,p1=iris.shape
    A1 = A.copy
    z=np.zeros(shape=(n1,p1))
    for i in range(0,p1):
        for j in range(0,n1):
            z[j,i] = np.mean(A[:,i])
    A1 = A - z        
    x=np.dot(A1.transpose(),A1)
    x=x/n1
    D, V = my_eigen(x)
    print D.round(6)
    print V.round(6)    
    print "The standard deviations of the components is:"
    for i in range(0,p1):
        print (math.sqrt(D[i]))

iris1 = datasets.load_iris()
iris = iris1.data[:,:]
my_pca(iris)