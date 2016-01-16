#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

/* Generate random uniform numbers using the linear congruential method */
// [[Rcpp::export]]
arma::vec runifC(double seed, int n) {
  
  /* This function takes in 2 inputs:
  seed: This is the seed used to generate the random numbers
  n: This is the number of random numbers we want to generate
  */
  
  double x = seed; // Create a variable x of type double. Initialize its value to seed.
  arma::vec u(n);  // Create a vector u of length n. This is the vector of random uniform
  // numbers we will be generating
  
  for(int i = 0; i < n; ++i){                                      
    x = fmod(pow(7,5) * x, pow(2,31));     // COMPLETE THIS LINE OF CODE
    u[i] = x / (pow(2,31) - 1.0);   // Normalize u[i] so that the values lie in [0, 1]
  }                                 // Remember that in C++, indexing starts at 0
  
  return(u); // This means we want our function to output the vector u.
  /* PLEASE NOTE THE FOLLOWING: we wrote "arma::vec" in front of
  our function name "runifC" on line 8. This is telling the program
  that our function is going to output something that is of type arma::vec.
  If I tried "return(x)", for example, we would have an error since 
  x is of type double. */
  
}


/* 
To test out the previous code, run the following line in R:
hist(runifC(12345, 10000))
The plot should look uniform
*/

/* Generate standard normal random variables using the polar transformation method */
// [[Rcpp::export]]
arma::vec rnormC(double seed, int n) {
  
  /* This function takes in 2 inputs:
  seed: This is the seed used to generate the random numbers
  n: This is HALF the number of random numbers we want to generate.
  Since this method generates an (X, Y) pair jointly, each iteration will
  output 2 standard normal variables (so n iterations generated 2n random numbers). 
  Of course, you can change this if you want.
  */
  
  double pi = 3.141592653589793238462643383280; // You can declare this globally as well
  double theta; 
  double r;
  arma::vec u(2);     // Create a vector u of length 2.
  arma::vec v(2 * n); // Create a vector v of length 2 * n. This vector stores our output.
  
  for(int i = 0; i < n; ++i){
    
    u = runifC(seed * (i + 1), 2); // Generate 2 random uniform numbers. Note: the first input 
    // changes on every iteration so we don't constantly generate 
    // the same random numbers. 
    
    theta = 2.0 * pi * u[0];  // u[0] is the first random number we generated
    r = sqrt(-2 * log(u[1])); // u[1] is the second random number we generated
    
    v[2 * i] = r * cos(theta) ;  // COMPLETE THIS LINE OF CODE
    v[(2 * i) + 1] = r * sin(theta);      // COMPLETE THIS LINE OF CODE
  }
  
  return(v);  // This means we want our function to output the vector v.
  
}

/* 
To test out the previous code, run the following line in R:
hist(rnormC(76543, 10000))
The plot should look standard normal
*/