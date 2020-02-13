#include <iostream>
#include <cmath>
#include <cfloat>
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
NumericVector electreC(NumericVector X, NumericVector weight_vect, int ncriteria, int ncols, int nrows, NumericVector concmat, NumericVector discmat){	
  int row1, col1, row2, col2;
  int i, j, k, m, n, cont;
  NumericVector mat(ncols*nrows);

  /* Creation of dynamic arrays in the memory*/

  double ***criteria_matrix;
  criteria_matrix = (double *** )malloc(ncriteria * sizeof(*criteria_matrix));
  if(criteria_matrix == NULL){
    /*Rprintf("Memory allocation fails!\n");*/
    exit(EXIT_FAILURE);
  }
  for(i = 0; i < ncriteria; i++){
    criteria_matrix[i] = (double **)malloc(ncols * sizeof(**criteria_matrix));
    if(criteria_matrix[i] == NULL){
      /*Rprintf("Memory allocation fails!\n");*/
      exit(EXIT_FAILURE);
    }
  }
  for(i = 0; i < ncriteria; i++){
    for (j = 0; j < ncols; j++){
      criteria_matrix[i][j] = (double *)malloc(nrows * sizeof(***criteria_matrix));
      if(criteria_matrix[i][j] == NULL){
        /*Rprintf("Memory allocation fails!\n");*/
        exit(EXIT_FAILURE);
      }
    }
  }

  NumericVector row_sum_conc(ncols * nrows) ; 
  NumericVector col_sum_conc(ncols * nrows) ;
  NumericVector row_sum_disc(ncols * nrows) ;
  NumericVector col_sum_disc(ncols * nrows) ;
  
  /* Initialisation of the vectors to 0 */ 
  for (n = 0; n < ncols * nrows ; ++n){
    row_sum_conc(n) = 0;
    col_sum_conc(n) = 0;
    row_sum_disc(n) = 0;
    col_sum_disc(n) = 0;
  }

  
  /* Rebuilding of the matrix*/
  m=0;
  for(i = 0; i < ncriteria; i++){
    for(j = 0; j < ncols; j++){
      for(k = 0; k < nrows; k++){
        criteria_matrix[i][j][k] = X(m++);
      }
    }
    
  }   

  /* Comparison pairwise */	
  k = 0;			
  for (row1 = 0; row1 < ncols; row1++)
  {
    for (col1 = 0; col1 < nrows; col1++)
    {
      j = 0;
      if (!ISNAN(criteria_matrix[0][row1][col1])){
        for (row2 = 0; row2 < ncols; row2++)
        {
          for (col2 = 0; col2 < nrows; col2++)
          {
            double conc = 0, disc = -100;
            for (i = 0; i < ncriteria; i++)
            {
              double d;
              d = criteria_matrix[i][row1][col1] - criteria_matrix[i][row2][col2];
              if (! ISNAN(d) && ! ISNAN(weight_vect[i])){
                if (d >= 0 ){
                  conc += weight_vect(i);
                }
                if (d >= disc){
                  disc = -d;
                }
              }
            }
            row_sum_conc(k) += conc;
            col_sum_conc(j) += conc;
            row_sum_disc(k) += disc;
            col_sum_disc(j) += disc;
            
            j++;	
          }
        }
      }
      else{
        concmat(k) =NAN;
        discmat(k) =NAN;
      }
      k++;
    }
  }
  
  /*calculate concordance and discordance index and storage in concmat et discmat */
  for (cont = 0; cont < ncols*nrows; cont++)
  {
    if (!ISNAN(concmat(cont))){
      /*fill matrix with concordance index */
      concmat(cont) = row_sum_conc(cont) - col_sum_conc(cont);
      /*fill matrix with discordance index */
      discmat(cont) = row_sum_disc(cont) - col_sum_disc(cont);
      
    }
  }	
  
  for(i=0; i<ncols * nrows; i++)
  {
    mat(i) = concmat(i) - discmat(i) ;
  }
  
  

  for(i = 0; i < ncriteria; i++){
    for (j = 0; j < ncols; j++){
      free(criteria_matrix[i][j]);
    }
  }
  for(i = 0; i < ncriteria; i++){
    free(criteria_matrix[i]);
  }
  free(criteria_matrix);
  
  return(mat);
  
}

