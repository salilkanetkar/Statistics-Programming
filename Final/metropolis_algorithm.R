library(Rcpp)
sourceCpp("metropolis_algorithm_cpp.cpp")
N=10^3
T = 10^2
chain = matrix(0,1,N)
chain1 =  metropolisCPP(chain, N, T)
hist(chain1,xlim=range(c(-25,25)),freq=FALSE)