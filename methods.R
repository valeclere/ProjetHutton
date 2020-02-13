######################################################################################
############################# ELECTRE method #########################################
######################################################################################
library(Rcpp)
sourceCpp("electre.cpp")

#Rcpp::sourceCpp("electre.cpp")
#' Electre algorithm calling C code
#' 
#' @param ras A rasterstack of criteria
#' @param weight A vector containing a weight for each criteria
#' @return the raster resulting of the Electre algorithm
#' @examples
#' A <-matrix(c(1,0,1,1,0,0,1,1),4,2)
#' B <-matrix(c(0,0,-1,0,-1,0,-1,0),4,2)
#' C <-matrix(c(0,0,1,1,0,0,0,1),4,2)
#' D <-stack(A,B,C) 
#' E <- electre(C, c(0.5,0.5,0.5))
#electre <- function(ras, weight){
 # print("You're Welcome !")
  #print(getwd())
  #X <- as.array(ras)
	## Normalisation of weights
	#if (sum(weight,na.rm=T)!=1) {weight <- weight/sum(weight, na.rm=T)}
	## C code read by column whereas R code read by row, so transposition of rows and columns
	#X <- aperm(X, c(2, 1, 3))
	#X.dim <- dim(X)
	#print("bonjour")
	## Creation of the output matrix (necessary to be as input in the C function)
	#concmat <- matrix(data=as.double(0), nrow(X), ncol(X))
	#discmat <- matrix(data=as.double(0), nrow(X), ncol(X))
	## Call to C code
	## NB: ncols is the original number of colums of the raster, so after transposition it corresponds to the number of rows in the C code 
	#out <- .C("electreC",X = as.double(X), weight_vect=as.double(weight), ncriteria = as.integer(X.dim[3]),
    #            ncols = as.integer(X.dim[2]), nrows = as.integer(X.dim[1]), concmat=concmat, discmat=discmat, NAOK = TRUE)
	## Transposition of the output matrix
	
#	concmat <- t(out$concmat)
#	discmat <- t(out$discmat)
#	elecmat <- concmat - discmat
#	#Creation of the final raster
#	#elecrast <- creationraster(elecmat, rst.pnl@extent)
#	elecrast <- creationraster(elecmat, ras@extent)
#	return(elecrast)
#}

electre <- function(ras, weight){
  X <- as.array(ras)
  # Normalisation of weights
  if (sum(weight,na.rm=T)!=1) {weight <- weight/sum(weight, na.rm=T)}
  # C code read by column whereas R code read by row, so transposition of rows and columns
  X <- aperm(X, c(2, 1, 3))
  X.dim <- dim(X)
  print("bonjour")
  # Creation of the output matrix (necessary to be as input in the C function)
  concmat <- matrix(data=as.double(0), nrow(X), ncol(X))
  discmat <- matrix(data=as.double(0), nrow(X), ncol(X))
  # Call to C code
  # NB: ncols is the original number of colums of the raster, so after transposition it corresponds to the number of rows in the C code 
  out <- electreC(as.double(X), as.double(weight), as.integer(X.dim[3]), as.integer(X.dim[2]), as.integer(X.dim[1]), concmat, discmat)
  # Transposition of the output matrix
  matr<- t(matrix(out,nrow=X.dim[1],ncol=X.dim[2]))
  #Creation of the final raster
  #elecrast <- creationraster(elecmat, rst.pnl@extent)
  elecrast <- creationraster(matr, ras@extent)
  return(elecrast)
}

####################### regime function ###########################################################


#' Variant of Regime algorithm calling C code
#' 
#' @param ras A rasterstack of criteria
#' @param weight A vector containing a weight for each criteria
#' @return the raster resulting of the Regime algorithm
#' @examples
#' A <-matrix(c(1,0,1,1,0,0,1,1),4,2)
#' B <-matrix(c(0,0,-1,0,-1,0,-1,0),4,2)
#' C <-matrix(c(0,0,1,1,0,0,0,1),4,2)
#' D <-stack(A,B,C)
#' E <- regime(C, c(0.5,0.5,0.5))
regime <- function(ras, weight){
	X <- as.array(ras)
	# Normalisation of weights
	weight <-weight/sum(weight, na.rm=T)	
	# C code read by column whereas R code read by row, so transposition of rows and columns
	X <- aperm(X, c(2, 1, 3))
	X.dim <- dim(X)
	# Creation of the output matrix (necessary to be as input in the C function)
	row_sum_regime <- matrix(data=as.double(0), nrow(X), ncol(X))
	# Call to C code
	out <- .C("regimeC", X = as.double(X), weight_vect=as.double(weight), ncriteria = as.integer(X.dim[3]),
                ncols = as.integer(X.dim[2]), nrows = as.integer(X.dim[1]), row_sum_regime=row_sum_regime, NAOK = TRUE)

	# Transposition of the output matrix
	row_sum_regime <- t(out$row_sum_regime)
	row_sum_regime <- row_sum_regime / (nrow(row_sum_regime) * ncol(row_sum_regime))
	#Creation of the final raster
	concrast <- creationraster(row_sum_regime, rst.pnl@extent)
	return(concrast)
}


################### creationraster function ####################################################


#' Create a raster with the OSGB36 projection and the extent given as parameter of a matrix
#' 
#' @param mat A matrix to be rasterised
#' @param extent The extent of the futur raster
#' @return the georeferenced raster resulting of the matrix
#' @examples
#' mat <-matrix(c(0,0,1,1,0,0,0,1),4,2)
#' ext <- extent(336000, 356000,786000,813500)
#' creationraster(mat, ext)
creationraster <- function(mat, extent){
	ras <- raster(mat)
	ras@extent <- extent
	OSGB36 <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")
	ras@crs <- OSGB36
	return(ras)
}


