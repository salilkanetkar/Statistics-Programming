import numpy as np

def mySweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    Det = 1
    if(n!=c):
        return "The matrix is not a square matrix. Please give a sqaure matrix as an input."
    for k in range(m):
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
        if(abs(A[k,k]) < 0.0001):
            return "The matrix is non invertible."
        Det = Det * A[k,k]
        A[k,k] = -1/A[k,k]
    print "The determinant is "+str(Det)+"."      
    return A
A = np.array([[1,2,3],[4,6,6],[7,8,9]], dtype=float).T
print mySweep(A, 3)

def myReverseSweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    Det = 1
    if(n!=c):
        return "The matrix is not a square matrix. Please give a sqaure matrix as an input."
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i != k and j != k:
                    A[i,j] = A[i,j]- A[i,k]*A[k,j]/A[k,k]

        for i in range(n):
            if i != k:
                A[i,k] = -A[i,k]/A[k,k]

        for j in range(n):
            if j != k:
                A[k,j] = -A[k,j]/A[k,k]
        if(abs(A[k,k]) < 0.0001):
            return "The matrix is non invertible."
        Det = Det * A[k,k]
        A[k,k] = -1/A[k,k]
    print "The determinant is "+str(Det)+"."      
    return A
C = np.array([[0.5,0.5,-0.83333],[0.5,-1,0.5],[-0.5,0.5,-0.166666]], dtype=float).T
print mySweep(C, 3)


def myGaussJordan(A, m):
    n = A.shape[0]
    B  = np.hstack((A, np.identity(n))) 
    for k in range(m):
        if abs(B[k,k]) < 0.0001:
            return "The matrix is non invertible." 
        a = B[k, k] 
        for j in range(n*2):
            B[k, j] = B[k, j] / a
        for i in range(n):
            if i != k:
                a = B[i, k]
                for j in range(n*2):
                    B[i, j] = B[i, j] - B[k, j]*a;
    return B

def myGaussJordanVec(A, m):
    n = A.shape[0]
    B  = np.hstack((A, np.identity(n)))  
    for k in range(m):
        if abs(B[k,k]) < 0.0001:
            return "The matrix is non invertible."
        B[k, :] = B[k, ] / B[k, k]
        for i in range(n):
            if i != k:
                B[i, ] = B[i, ] - B[k, ]*B[i, k];
    
    return B

A = np.array([[1,2,3],[4,5,6],[7,8,9]], dtype=float).T
print myGaussJordan(A, 3)
print myGaussJordanVec(A, 3)