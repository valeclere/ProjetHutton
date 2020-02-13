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
NumericVector miniMatrix(NumericMatrix x, int longueurSearching ,int longueurAbove) {
  NumericVector out(longueurSearching) ; 
  for (int i=0 ; i<longueurSearching ; i++){
    double a=x(i,0) ;
    for (int j=0 ; j<longueurAbove ; j++) {
      if (x(i,j)<a) {
        a=x(i,j) ;
      }
    } 
    out(i)=a ; 
  }
  
  return(out);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
  