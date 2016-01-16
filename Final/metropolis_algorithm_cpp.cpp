#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec runifC(double seed, int n) 
{
  double x = seed;
  arma::vec u(n); 
  for(int i = 0; i < n; ++i)
  {                                      
    x = fmod(pow(7,5) * x, pow(2,31)-1.0);    
    u[i] = x / (pow(2,31) - 1.0);  
  }                                
  return(u);
}


// [[Rcpp::export]]
double f(long x, int variance)
{
  return (exp(-pow(x,2)/(2*variance)));
}

// [[Rcpp::export]]
double alpha(long x, long y, int variance = 2)
{
  return (1.0<(f(y,variance)/f(x,variance))) ? 1.0 : (f(y,variance)/f(x,variance));
}

// [[Rcpp::export]]
arma::vec metropolisCPP(arma::vec ch, long N, long T) 
{
  //arma::mat ch = clone(chain);
  long y;
  for(int n=0;n<N;n++)
  {
    long x=0;
    for(int i=0;i<T;i++)
    {
      arma::vec u1(10001);
      u1 = runifC(12345,10001);
      if(u1(n)>0.5)
      {
        y = x + 1;
      }
      else
      {
        y = x - 1;
      }
      arma::vec u2 = runifC(123,10001);
      int accept = (u2[n]<alpha(x,y));
      if(accept == 1)
      {
        x = y;
      }
      else
      {
        x = x;
      }
    }
    ch[n] = x;
  }
  return ch;
}