"
Name: Salil Kanetkar
UCLA ID: 704557096
QR function has been written using the Gram Schmidt Method.
Final answer for PCA has been verified using inbuilt function.
"

myeigen <- function(A)
{
  T = 1000
  n = nrow(A)
  V = matrix(rnorm(n*n), nrow=n)
  
  for (i in 1:T)
  {
    V = myqr_gram_schmidt(V)$Q
    V = A %*% V
  }
  
  B = myqr_gram_schmidt(V)
  
  result <- list(B$Q, diag(B$R))
  names(result) <- c("vectors", "values")
  result
}

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

my_pca <- function(A)
{
  n1=nrow(A)
  p1=ncol(A)
  A1= A
  for(i in 1:p1)
  {
    A1[,i]=A[,i]-mean(A[,i])
  }
  x = t(A1) %*% A1/n1
  pca_answer <- myeigen(x)
  print (pca_answer)
  print ("The standard deviations of the components is:")
  print (sqrt(pca_answer$values))
}

my_pca_inbuilt <- function(A)
{
  print ("Comparing with the standard deviation values from inbuilt function:")
  irispca<-princomp(A)
  summary(irispca)
}

load(iris)
iris1 = iris[-5]
iris_matrix = as.matrix(iris1)
my_pca (iris_matrix)
my_pca_inbuilt(iris1)