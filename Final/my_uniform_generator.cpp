#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector generateUniformRandomNumbers(NumericVector y,int it, int a, int c, long m) 
{
  NumericVector x=clone(y);
  int i;
  for(i=0;i<=it-2;i++)
  {
    x[i+1] = fmod((a*x[i] + c), m);
  }
  return x;
}