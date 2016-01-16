library(Rcpp)
sourceCpp("mySweep_new_cpp.cpp")

myRidgeCpp <- function(X, Y, lambda)
{
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweep_cpp(A, p+1)
  beta = S[1:(p+1), p+2]
  return(beta)
}

error <- function(Yhat,Y)
{
  return (mean((Yhat-Y)^2))
}

error_calculate <- function(X_train, Y_train, X_test, Y_test, lambda)
{
  beta = myRidgeCpp(X_train, Y_train, lambda)
  Yhat_train = cbind(rep(1, n_train), X_train)%*%beta
  Yhat_test = cbind(rep(1, n_test), X_test)%*%beta
  Y_total = list(Yhat_train,Yhat_test)
  return (Y_total)
}

  n = 100
  n_train = ceiling(2*n/3)
  n_test = n-n_train
  p = 500
  sigma = .1
  lambda = 10.
  x = runif(n)
  x_train = sample(x, size = n_train, replace = FALSE)
  y_train = x_train^2 + rnorm(n_train)*sigma
  Y_train = matrix(y_train, nrow=n_train)
  x_test = setdiff(x,x_train)
  y_test = x_test^2 + rnorm(n_test)*sigma
  Y_test = matrix(y_test, nrow=n_test)
  X_train = matrix(x_train, nrow=n_train)
  X_test = matrix(x_test, nrow=n_test)
  error_test = NULL
  error_train = NULL
  for (k in (1:(p-1))/p){
    X_train = cbind(X_train, (x_train>k)*(x_train-k))
    X_test = cbind(X_test, (x_test>k)*(x_test-k))
  }
  Y_total=NULL
  for(lambda in 1:100){
    Y_total=error_calculate(X_train, Y_train, X_test, Y_test, lambda)
    Yhat_train = Y_total[[1]]
    Yhat_test = Y_total[[2]]
    error_train = append(error_train,error(Yhat_train,Y_train))
    error_test = append(error_test,error(Yhat_test,Y_test))
  }
  lambda1 = 1:100

  plot(lambda1,error_train,col="red",xlab="Lambda",ylab="Error",type="p")
  par(new = TRUE)
  plot(lambda1,error_test,col="green",xlab="Lambda",ylab="Error",type="p")
  legend(2000,9.5,  c("Train Error","Test Error"), # puts text in the legend
         
         lty=c(1,1), # gives the legend appropriate symbols (lines)
         
         lwd=c(2.5,2.5),col=c("red","green"))