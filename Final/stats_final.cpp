/*
Name: Salil Kanetkar
UID: 704557096
STATS 202A Final Homework
This file has all the CPP Code of the homework
*/

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


//Problem 1: Random Number Generator for Uniform Distribution
// [[Rcpp::export]]
arma::vec runifC(double seed, int n) 
{
  double x = seed;
  arma::vec u(n); 
  for(int i = 0; i < n; ++i)
  {                                      
    x = fmod(pow(7,5) * x, pow(2,31) - 1.0);
    u[i] = x / (pow(2,31) - 1.0);
  }
  return(u);
}


//Problem 1: Random Number Generator for Normal Distribution
// [[Rcpp::export]]
arma::vec rnormC(double seed, int n) 
{
  double pi = 3.141592653589793238462643383280;
  double theta; 
  double r;
  arma::vec u(2);     
  arma::vec v(2 * n);
  for(int i = 0; i < n; ++i)
  {
    u = runifC(seed * (i + 1), 2);
    theta = 2.0 * pi * u[0];
    r = sqrt(-2 * log(u[1]));
    v[2 * i] = r * cos(theta);
    v[(2 * i) + 1] = r * sin(theta);
  }
  return(v);
}



//Problem 2: Metropolis Algorithm
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



//Problem 3: Gibbs Sampler (Bivariate Normal Distribution)
// [[Rcpp::export]]
arma::cube gibbsCPP(NumericVector p2, int T, int M, double rho, double x0, double y0) 
{
  IntegerVector dim_p2=p2.attr("dim");
  arma::cube p(p2.begin(), dim_p2[0], dim_p2[1], dim_p2[2]);
  double x, y;
  arma::vec n1 = rnormC(76543,M*T);
  //arma::vec n2 = rnormC(76543,M*T);
  for(int m=0;m<M;m++)
  {
    x = x0;
    y = y0;
    p(0,0,m) = x;
    p(1,0,m) = y;
    for(int t=1;t<T;t++)
    {
      x = n1[m*T];
      x = x * sqrt(1-pow(rho,2)) + rho * y;
      y = n1[m*T+1];
      y = y * sqrt(1-pow(rho,2)) + rho * x;
      p(0,t,m) = x;
      p(1,t,m) = y; 
    }
  }
  return(p);
}