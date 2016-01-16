#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

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
    //v[2 * i] = sd1 * v[2 * i] + mean1;
    v[(2 * i) + 1] = r * sin(theta);
    //v[(2 * i) + 1] = sd2 * v[(2 * i) + 1] + mean2;
  }
  return(v);
}

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
      //x = rnormC(1,M*T)[m*t];
      //x = x * sqrt(1-pow(rho,2)) + rho * y;
      //y = rnormC(76543,M*T)[(m*t)+1];
      //y = y * sqrt(1-pow(rho,2)) + rho * x;
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