"
Name: Salil Kanetkar
UID: 704557096
STATS 202A Final Homework
This file has all the R Code of the homework
"


#Problem 1: Random Number Generator for Uniform Distribution
library(Rcpp)
sourceCpp("stats_final.cpp")
par(mfrow=c(1,2))
hist(runifC(12345, 10000), xlab ="U",prob=T,main="Histogram of U")
plot(runifC(12345, 10000)[seq(2,1000,by=2)], runifC(12345, 10000)[seq(3,1001,by=2)] , xlab = expression('x'[2*t]), ylab = expression('x'[2*t+1]), main="catterplot")


#Problem 1: Random Number Generator for Normal Distribution
library(Rcpp)
sourceCpp("stats_final.cpp")
hist(rnormC(76543, 10000),xlab="X and Y",main = "Histogram of X and Y", xlim =c(-4,4))
normal = rnormC(76543,10000)
x = normal[seq(1,20000,by=2)]
y = normal[seq(2,20000,by=2)]
par(mfrow =c(1,2))
hist(x , breaks =30, prob =T, xlim =c(-4,4) , xlab ="X", main = "Histogram of X")
curve(dnorm(x,mean =0, sd =1) ,lwd =2, add =T, col="blue")
hist(y , breaks =30, prob =T, xlim =c(-4,4), xlab ="Y", main = "Histogram of Y")
curve(dnorm(x,mean =0, sd =1) ,lwd =2, add =T, col="blue")

#Problem 2: Metropolis Algorithm
library(Rcpp)
sourceCpp("stats_final.cpp")
N=10^3
T = 10^2
chain = matrix(0,1,N)
chain1 =  metropolisCPP(chain, N, T)
hist(chain1,xlim=range(c(-25,25)),freq=FALSE)


#Problem 3: Gibbs Sampler (Bivariate Normal Distribution)
library(Rcpp)
sourceCpp("stats_final.cpp")
gibbs<-function (T=1000, M=100, rho, x0, y0) 
{
  p<-rep(0,2*M*T) #Allocate memory for results
  dim(p)<-c(2,T,M)
  p1 = gibbsCPP(p,T=1000,M=100,rho,x0,y0) 
  p1
}

#making a movie
library(animation)
rho <- 0.99
M=100
par(mar=c(2,2,1,2), mfrow=c(3,3))
bvn <- gibbs(x0=-5,y0=0,M=M,rho=rho)
ani.options(convert = "C:\\Program Files\\ImageMagick-6.9.2-Q16\\convert.exe")
lims <- 8*c(-1,1)
for (t in 1:9){ plot(bvn[1,t,],bvn[2,t,],
                     xlim=lims, ylim=lims,
                     col=1:M,
                     pch=16, main=paste('t =',t))
  ani.pause(.2)
}#saving as GIF file
saveGIF({ for (t in 1:9){ plot(bvn[1,t,],bvn[2,t,], xlim = lims, ylim = lims, col=1:M,
                               pch =16, main = paste('t =',t))
}
}, movie.name = "bvn_gibbs_rho.gif" )