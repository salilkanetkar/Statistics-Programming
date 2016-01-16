library(Rcpp)
library('RcppArmadillo')
sourceCpp("gibbs_sampling_cpp.cpp")
gibbs<-function (T=1000, M=100, rho, x0, y0) 
{
  p<-rep(0,2*M*T) #Allocate memory for results
  dim(p)<-c(2,T,M)
  p1 = gibbsCPP(p,T=1000,M=100,rho,x0,y0) 
  p1
}



#making a movie
library(animation)
rho <- 0.1
M=100
par(mar=c(2,2,1,2), mfrow=c(3,3))
bvn <- gibbs(x0=-5,y0=0,M=M,rho=rho)
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
}, movie.name =paste("bvn_gibbs_rho_", rho))