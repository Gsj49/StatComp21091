#include <Rcpp.h>
using namespace Rcpp;


// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar





//' This (cpp) function generates a bivariate chain according to homework_10
//'
//' @param N length of the chain
//' @export
// [[Rcpp::export]]
NumericMatrix cppbivariate_chain(int N) {
  NumericMatrix X(N,2);
  int a = 2;
  int b = 3;
  int n = 10;
  X(0,0) = 2.5;
  X(0,1) = 0.5;
  for (int i = 1; i < N; i++) {
    X(i,0) = R::rbinom(n,X(i-1,1));
    X(i,1) = R::rbeta(X(i-1,0)+a, n-X(i-1,0)+b);
  }
  return(X);
}
