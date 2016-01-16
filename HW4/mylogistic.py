'''
Name: Salil Kanetkar
UCLA ID: 704557096
The data set has been directly imported from the URL. Hence while running the code,
it is necessary that the internet is connected to.
'''
import numpy as np
import csv as csv
import urllib2
import math

#Function for Logistic Regression
def mylogistic(_x, _y):
    x = _x.copy()
    y = _y.copy()
    r, c = x.shape
    r1, c1= y.shape
    beta = np.zeros((c, 1))
    epsilon = 1e-6
    while True:
        eta = np.dot(x, beta)
        pr = exp_it(eta)
        w = pr * (1 - pr)
        z = eta + (y - pr) / w
        sw = np.sqrt(w)
        mw = np.repeat(sw, c, axis=1)
        x_work = mw * x
        y_work = sw * z
        beta_new = mylm(x_work, y_work)
        err = np.sum(np.abs(beta_new - beta))
        beta = beta_new
        if err < epsilon:
            break
    return beta

#Function for Linear Regression
def mylm(X,Y):
    n2,p=X.shape
    Z = np.concatenate((X, Y), 1)
    _, R = myqr(Z)
    Y1=np.ones(shape=(6,1))
    R1 = R[0:p, 0:p]#check
    #print R1.shape
    Y1[:,0] = R[0:p, p]#check
    beta = solve1(R1, Y1)
    return beta

#Function to Calculate the Beta Value
def solve1(R1,Y1):
    n,p = R1.shape
    Z1=np.concatenate((R1[0:p,0:p],Y1),axis=1)
    intr = np.ones(shape=(1,1))
    Z2=np.concatenate([Y1.transpose(),intr],axis=1)
    Z=np.concatenate((Z1,Z2),axis=0)
    X=mySweep(Z,p)
    res=np.ones(shape=(6,1))
    res[:,0]=X[0:p,p]     
    return(res)

#Function to calculate the sweep
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

#QR Decomposition using Gram Schmidt Method
def myqr(A):
    n,p = A.shape
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
    sq1=np.zeros(shape=(n,1))
    temp=np.zeros(shape=(n,1))
    Q=np.zeros(shape=(n,p))
    for i in range(0,p):
        temp=np.dot(U[:,i],U[:,i])
        sq1[i,0]=math.sqrt(np.sum(temp))
        Q[:,i]=U[:,i]/sq1[i,0]
    R = np.dot(Q.transpose(),A)
    return Q, R
 
def exp_it(_x):
    x = _x.copy()
    y = 1 / (1 + np.exp(-x))
    return y

#Importing and pre-processing the data
url = 'http://www.ats.ucla.edu/stat/data/binary.csv'
response = urllib2.urlopen(url)
cr = csv.reader(response,delimiter=',') 
x2=list(cr)
result=np.array(x2)
result=result[1:,:]
result=result.astype(np.float)
n1,p1=result.shape
admit=np.ones(shape=(n1,1))
admit[:,0]=result[:,0]
rank=np.zeros(shape=(n1,3))
for i in range(0,n1):
    if(result[i,3] == 2):
        rank[i,0]=1
    if(result[i,3] == 3):
        rank[i,1]=1
    if(result[i,3] == 4):
        rank[i,2]=1
intercept=np.ones(shape=(n1,1))
result=result[:,1:3]
mydata = np.concatenate((intercept, result, rank), 1)
beta = mylogistic(mydata,admit)
print beta