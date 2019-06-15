#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector myfunction_jugdeCpp(DataFrame df) {
  int t = df.nrows();
  NumericVector v (1);
  NumericVector v1 = df[0];
  NumericVector v2 = df[1];
  for(int i = 1; i < t; i++){
    int i_2 = i - 1; 
    double res = sqrt(pow((v1[i] - v1[i_2]), 2.0) + pow((v2[i] - v2[i_2]), 2.0)); 
    v.push_back(res);
  }
  return v;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// /*** R
// timesTwo(42)
// */
