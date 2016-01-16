mySweep <- function(A, m)
{
n <- dim(A)[1]
Determinant <- 1
if(dim(A)[1]!=dim(A)[2])
  return("Please return a square matrix")
for (k in 1:m) 
 {
  for (i in 1:n)     
   for (j in 1:n)   
      if (i!=k  & j!=k)     
         A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
    
  for (i in 1:n) 
    if (i!=k) 
      A[i,k] <- A[i,k]/A[k,k]  
 
  for (j in 1:n) 
    if (j!=k) 
      A[k,j] <- A[k,j]/A[k,k]  
    if (abs(A[k,k]) < 0.0001)
        return ("The matrix is non invertible.")  
  Determinant <- Determinant * A[k,k]  
  if(A[k,k])
  A[k,k] <- - 1/A[k,k] 
}
print(Determinant)
return(A)
}


A = matrix(c(1,2,3,4,6,6,7,8,9), 3,3)
#solve(A)
mySweep(A,3)


myReverseSweep <- function(A, m)
{
  n <- dim(A)[1]
  if(dim(A)[1]!=dim(A)[2])
    return("Please return a square matrix")
  for (k in 1:m) 
  {
    for (i in 1:n)     
      for (j in 1:n)   
        if (i!=k  & j!=k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for (i in 1:n) 
          if (i!=k) 
            A[i,k] <- -A[i,k]/A[k,k]  
          
          for (j in 1:n) 
            if (j!=k) 
              A[k,j] <- -A[k,j]/A[k,k]  
            if (abs(A[k,k]) < 0.0001)
              return ("The matrix is non invertible.")  
            if(A[k,k])
              A[k,k] <- - 1/A[k,k] 
  }
  return(A)
}
C = matrix(c(0.5,0.5,-0.83333,0.5,-1,0.5,-0.5,0.5,-0.1677), 3,3)
#solve(A)
myReverseSweep(C,3)


myGaussJordan <- function(A, m)
{
n <- dim(A)[1]
B <- cbind(A, diag(rep(1, n)))
 
for (k in 1:m) 
 {
  a <- B[k, k]
  for (j in 1:(n*2))     
     B[k, j] <- B[k, j]/a
  for (i in 1:n)
     if (i != k)
     {
        a <- B[i, k]
        for (j in 1:(n*2))
           B[i, j] <- B[i, j] - B[k, j]*a; 
    }    
 }
return(B)
}

A = matrix(c(1,2,3,7,11,13,17,21,23), 3,3)
solve(A)
myGaussJordan(A,3)

myGaussJordanVec <- function(A, m)
{
n <- dim(A)[1]
B <- cbind(A, diag(rep(1, n)))
 
for (k in 1:m) 
 {
  B[k, ] <- B[k, ]/B[k, k]
  for (i in 1:n)
     if (i != k)
        B[i, ] <- B[i, ] - B[k, ]*B[i, k];   
 }
return(B)
}

A = matrix(c(1,2,3,7,11,13,17,21,23), 3,3)
solve(A)
myGaussJordanVec(A,3)
