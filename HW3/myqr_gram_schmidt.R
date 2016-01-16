myqr_gram_schmidt <- function(A)
{
  n=nrow(A)
  p=ncol(A)
  Q=matrix(nrow=n,ncol=p)
  R=matrix(nrow=n,ncol=p)
  U=matrix(0,nrow=n,ncol=p)
  U[,1]=A[,1]
  for (k in 2:p)
  {
    proj=matrix(0,nrow=n,ncol=1)
    for(i in 1:(k-1))
    {
      a=sum(A[,k]*U[,i])
      b=sum(U[,i]*U[,i])
      c=a/b
      ck=c*U[,i]
      proj = proj + ck
    }  
    U[,k]=A[,k]-proj[,1]
  }
  sq = matrix(0,n,1)
  temp=matrix(0,nrow=n,ncol=1)
  Q=matrix(0,nrow=n,ncol=p)
  for(i in 1:p)
  {
    temp=U[,i]*U[,i]
    sq[i,1]=sqrt(sum(temp))
    Q[,i]=U[,i]/sq[i,1]
  }
  R= t(Q) %*% A
  result <- list(Q, R)
  names(result) <- c("Q", "R")
  result
}

A=matrix(c(12,6,-4,-51,167,24,4,-68,-41),nrow=3,ncol=3)
result=myqr_gram_schmidt(A)
print (result)