#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector my_runif(NumericVector x)
{
  //int it, int a, int c, long m, int seed
  int it = 10000;
  int a = pow(7,11);
  int c = 0;
  long m = pow(2,31)-1;
  int i;
  for(i=0;i<=it-2;i++)
  {
    x[i+1] = fmod((a*x[i] + c), m);
  }
  return x;
}




// [[Rcpp::export]]
List generateNormalRandomNumbers(int it,long num, NumericVector result, NumericVector unif2)
{
  NumericVector r=clone(result);
  NumericVector unif1=clone(unif2);
  int i;
  for(i=0;i<it;i++)
  {
    double Z;
    long rejected = 0;
    double Y = -log(unif1[i]);
    
    double U = unif1[it-i-1];
    int counter = 0;
    while (U >= exp(-pow((Y-1),2)/2))
    {
      rejected++;
      Y = -log(unif1[counter]);
      U = unif1[it-counter-1];
      counter++;
      if(counter == it)
      {
        counter = 0;
      }
    } 
    num = num + rejected;
    if (unif1[i+1] < 0.5)
    {
      Z = Y;
    }
    else
    {
      Z = -Y;
    }
    r[i] = Z;
  }
  List ans;
  ans["result"] = r;
  ans["num"] = num;
  return(ans);
}