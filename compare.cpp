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
LogicalVector compare(IntegerMatrix x, IntegerMatrix y) {
    int lengthx = x.nrow() ; 
    int lengthy = y.nrow() ;
    LogicalVector out(lengthx) ;
    for(int k=0 ; k<lengthx  ; k++ ){
      out(k)= FALSE ;
    }
    
    for (int i=0 ; i<lengthx ; i++) {
      for (int j=0 ; j<lengthy ; j++){
        if (x(i,0)==y(j,0) and x(i,1)==y(j,1)) {
          out(i)=TRUE ; 
        }
      }
    }
    return(out) ;
    
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


