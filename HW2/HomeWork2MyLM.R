"
Name: Salil Kanetkar
UCLA ID: 704557096
After importing the data set, it has been normalized.
"
mySweep <- function(A, m){
  n <- dim(A)[1]
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
            A[k,k] <- - 1/A[k,k] 
  }
  return(A)
}

myqr <- function(A){
  n = nrow(A)
  m = ncol(A)
  R = A
  Q = diag(n)
  for (k in 1:(m-1))
  {
    x = matrix(rep(0, n), nrow = n)
    x[k:n, 1] = R[k:n, k]
    g = sqrt(sum(x^2))
    v = x
    v[k] = x[k] + sign(x[k,1])*g
    
    s = sqrt(sum(v^2))
    if (s != 0)
    {
      u = v / s
      R = R - 2 * u %*% t(u) %*% R
      Q = Q - 2 * u %*% t(u) %*% Q
    }
  }
  result <- list(t(Q), R)
  names(result) <- c("Q", "R")
  result
}

solve1 <- function(X,Y){
  n = nrow(X)
  p = ncol(X)
  X1 <- cbind(X[1:p,1:p],Y)
  X2 <- cbind(t(Y),1)
  XX <- rbind(X1,X2)
  S = mySweep(XX,p)
  print(ncol(S))
  S1 = S[,(p+1):ncol(S)]
  return(S1)
}

mylm_sweep<-function(X,Y){
  n=nrow(X)
  p=ncol(X)
  z=cbind(rep(1,n),X,Y)
  A=t(z)%*%z
  S=mySweep(A,p+1)
  beta_hat=S[1:(p+1),p+2]
  return(beta_hat)
}

mylm_qr <- function(X,Y){
  n=nrow(X)
  p=ncol(X)
  Z = cbind(rep(1, n), X, Y)
  R = myqr(Z)$R
  R1 = R[1:(p+1), 1:(p+1)]
  Y1 = R[1:(p+1), p+2]
  beta = solve1(R1, Y1)
  return(beta)
}

library('matrixStats')
state_x77 <- read.csv("H:/UCLA Fall 2015/202 - Statistics Programming/HW2/state_x77.csv")
mat <- data.matrix(state_x77, rownames.force = NA)
mat <- mat[,-1]
col_sd <- colSds(mat)
col_mean <- colMeans(mat)
mat1 <- matrix(nrow=50,ncol=9)
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    mat1[i,j] <- (mat[i,j]-col_mean[j])/col_sd[j]
  }
}
X <- mat1[,c(1,2,3,5,6,7,8,9)]
Y <- mat1[,c(4)]

beta = mylm_qr(X,Y)
beta_hat = mylm_sweep(X,Y)
print(beta)
print(beta_hat)
lm(Y~X)