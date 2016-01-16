library(Rcpp)
sourceCpp("my_uniform_generator.cpp")
lcg_cpp <- function(a,c,m,run.length,seed)
{
  x <- rep(0,run.length)
  x[1]<- seed
  X = generateUniformRandomNumbers(x,run.length,a,c,m)
  U<- X/m #scale all of x to be between 0 and 1
  return(list(X=X,U=U))
}
z = lcg_cpp(7^5,0,2^31-1,10000,1)
par(mfrow=c(1,2))
hist(z$U,xlab ="U",prob=T,main="Histogram of U")
plot(z$X[seq(2,1000,by=2)], z$X[seq(3,1001,by=2)] ,
     xlab = expression('x'[2*t]), ylab = expression('x'[2*t+1]), main="scatterplot")