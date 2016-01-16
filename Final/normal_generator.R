library(Rcpp)
sourceCpp("my_normal_generator.cpp")
num =0
iter = 10000
result =rep(0,iter)

x <- rep(0,10000)
x[1]<- 4.01
m = 2^31-1
X = my_runif(x)
U = X/m



answer = generateNormalRandomNumbers(iter,num,result,U)
result = answer$result
num = answer$num
rejection_rate = num/(iter+num)
acceptance_rate = 1-rejection_rate
print(rejection_rate)
hist(result, breaks = 30, prob=T, xlab = "Z",
     main = paste("Histogram of Z, Acceptance rate= ", round(acceptance_rate,2)))
curve(dnorm(x,mean=0,sd=1), lwd=2, add=T, col="blue")
