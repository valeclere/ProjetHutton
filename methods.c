#include <R.h>
#include <math.h>

void electreC(double *X, double *weight_vect, int *ncriteria, int *ncols, int *nrows, double *concmat, double *discmat){	
  int row1, col1, row2, col2;
  int i, j, k, m, n, cont;
  /* Creation of dynamic arrays in the memory*/
  double *row_sum_conc, *col_sum_conc, *row_sum_disc, *col_sum_disc;
  row_sum_conc = malloc(*ncols * *nrows * sizeof(*row_sum_conc));
  col_sum_conc = malloc(*ncols * *nrows * sizeof(*col_sum_conc));
  row_sum_disc = malloc(*ncols * *nrows * sizeof(*row_sum_disc));
  col_sum_disc = malloc(*ncols * *nrows * sizeof(*col_sum_disc));
  double ***criteria_matrix;
  criteria_matrix = malloc(*ncriteria * sizeof(*criteria_matrix));
  if(criteria_matrix == NULL){
    /*Rprintf("Memory allocation fails!\n");*/
    exit(EXIT_FAILURE);
  }
  for(i = 0; i < *ncriteria; i++){
    criteria_matrix[i] = malloc(*ncols * sizeof(**criteria_matrix));
    if(criteria_matrix[i] == NULL){
      /*Rprintf("Memory allocation fails!\n");*/
      exit(EXIT_FAILURE);
    }
  }
  for(i = 0; i < *ncriteria; i++){
    for (j = 0; j < *ncols; j++){
      criteria_matrix[i][j] = malloc(*nrows * sizeof(***criteria_matrix));
      if(criteria_matrix[i][j] == NULL){
        /*Rprintf("Memory allocation fails!\n");*/
        exit(EXIT_FAILURE);
      }
    }
  }
  
  
  /* Initialisation of the vectors to 0 */ 
  for (n = 0; n < *ncols * *nrows; ++n){
    row_sum_conc[n] = 0;
    col_sum_conc[n] = 0;
    row_sum_disc[n] = 0;
    col_sum_disc[n] = 0;
  }
  
  
  /* Rebuilding of the matrix*/
  m=0;
  for(i = 0; i < *ncriteria; i++){
    for(j = 0; j < *ncols; j++){
      for(k = 0; k < *nrows; k++){
        criteria_matrix[i][j][k] = X[m++];
      }
    }
    
  }   
  
  /* Comparison pairwise */	
  k = 0;			
  for (row1 = 0; row1 < *ncols; row1++)
  {
    for (col1 = 0; col1 < *nrows; col1++)
    {
      j = 0;
      if (!isnan(criteria_matrix[0][row1][col1])){
        for (row2 = 0; row2 < *ncols; row2++)
        {
          for (col2 = 0; col2 < *nrows; col2++)
          {
            double conc = 0, disc = -100;
            for (i = 0; i < *ncriteria; i++)
            {
              double d;
              d = criteria_matrix[i][row1][col1] - criteria_matrix[i][row2][col2];
              if (!isnan(d) && !isnan(weight_vect[i])){
                if (d >= 0 ){
                  conc += weight_vect[i];
                }
                if (d >= disc){
                  disc = -d;
                }
              }
            }
            row_sum_conc[k] += conc;
            col_sum_conc[j] += conc;
            row_sum_disc[k] += disc;
            col_sum_disc[j] += disc;
            
            j++;	
          }
        }
      }
      else{
        concmat[k] =NAN;
        discmat[k] =NAN;
      }
      k++;
    }
  }
  
  /*calculate concordance and discordance index and storage in concmat et discmat */
  for (cont = 0; cont < *ncols* *nrows; cont++)
  {
    if (!isnan(concmat[cont])){
      /*fill matrix with concordance index */
      concmat[cont] = row_sum_conc[cont] - col_sum_conc[cont];
      /*fill matrix with discordance index */
      discmat[cont] = row_sum_disc[cont] - col_sum_disc[cont];
      
    }
  }	
  
  /* Free memory */
  free(row_sum_conc);
  free(col_sum_conc);
  free(row_sum_disc);
  free(col_sum_disc);
  for(i = 0; i < *ncriteria; i++){
    for (j = 0; j < *ncols; j++){
      free(criteria_matrix[i][j]);
    }
  }
  for(i = 0; i < *ncriteria; i++){
    free(criteria_matrix[i]);
  }
  free(criteria_matrix);
  
}



void regimeC(double *X, double *weight_vect, int *ncriteria, int *ncols, int *nrows, double *row_sum_regime){	
  int row1, col1, row2, col2;
  int i, j, k, m;
  /* Creation of a dynamic array in the memory*/
  double ***criteria_matrix;
  criteria_matrix = malloc(*ncriteria * sizeof(*criteria_matrix));
  if(criteria_matrix == NULL){
    /*Rprintf("Memory allocation fails!\n");*/
    exit(EXIT_FAILURE);
  }
  for(i = 0; i < *ncriteria; i++){
    criteria_matrix[i] = malloc(*ncols * sizeof(**criteria_matrix));
    if(criteria_matrix[i] == NULL){
      /*Rprintf("Memory allocation fails!\n");*/
      exit(EXIT_FAILURE);
    }
  }
  for(i = 0; i < *ncriteria; i++){
    for (j = 0; j < *ncols; j++){
      criteria_matrix[i][j] = malloc(*nrows * sizeof(***criteria_matrix));
      if(criteria_matrix[i][j] == NULL){
        /*Rprintf("Memory allocation fails!\n");*/
        exit(EXIT_FAILURE);
      }
    }
  }
  
  
  /* Rebuilding of the matrix*/
  m=0;
  for(i = 0; i < *ncriteria; i++){
    for(j = 0; j < *ncols; j++){
      for(k = 0; k < *nrows; k++){
        criteria_matrix[i][j][k] = X[m++];
      }
    }
    
  }   
  
  /* Comparison pairwise */	
  k = 0;			
  for (row1 = 0; row1 < *ncols; row1++)
  {
    for (col1 = 0; col1 < *nrows; col1++)
    {           
      if (!isnan(criteria_matrix[0][row1][col1])){
        for (row2 = 0; row2 < *ncols; row2++)
        {
          for (col2 = 0; col2 < *nrows; col2++)
          {
            if (!isnan(criteria_matrix[0][row2][col2])){
              double reg = 0;
              for (i = 0; i < *ncriteria; i++)
              {
                double d;
                d = criteria_matrix[i][row1][col1] - criteria_matrix[i][row2][col2];
                if (!isnan(d) && !isnan(weight_vect[i])){
                  if (d >= 0 ){
                    reg += (1 * weight_vect[i]);
                  }
                  else if (d <0){
                    reg += (-1 * weight_vect[i]);
                  }
                  else {
                    reg +=0;
                  }
                }
              }
              row_sum_regime[k] += reg;
            }
          }
        }
      }
      else{
        row_sum_regime[k] =NAN;
      }
      k++;
    }
  }
  
  /* Free memory */
  for(i = 0; i < *ncriteria; i++){
    for (j = 0; j < *ncols; j++){
      free(criteria_matrix[i][j]);
    }
  }
  for(i = 0; i < *ncriteria; i++){
    free(criteria_matrix[i]);
  }
  free(criteria_matrix);
  
}
