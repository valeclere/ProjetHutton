library(shiny)
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(plotGoogleMaps)
library(RColorBrewer)
library(Rcpp)

Rcpp::sourceCpp("miniMatrix.cpp")
Rcpp::sourceCpp("compare.cpp")

######################################################################################################################
############### 'lwc' is a FORMER function for LWC method. You will use only 'lwc2' function now. ####################
######################################################################################################################

#' Linear Weight Combinaison distinguishing the positive and negative criteria rasters
#' 
#' @param rasn A rasterstack containing the negative criteria raster
#' @param rasp A rasterstack containing the positive criteria raster
#' @param weightn A vector containing the weights for the negative criteria
#' @param weightp A vector containing the weights for the positive criteria
#' @return the raster resulting of the LWC method
#' @examples
#' r1 <- raster(matrix(runif(100, min=-1, max=0),nrow=10, ncol=10))
#' r2 <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' w1 <- c(0.5)
#' w2 <- c(0.8)
#' lwc(r1,r2,w1,w2)


#lwc = function(rasn, rasp, weightn, weightp){
#	n_pl <- length(x1p)
#	n_nl <- length(x1n)
#	
#	#normalization of the weights:  sum of positive criteria weights=sum of negative criteria weights=1
#	if (sum(weightp,na.rm=T)>=1) weightp=(weightp/sum(weightp,na.rm=T))-0.01 else weightp=weightp
#	if (sum(weightn,na.rm=T)>=1) weightn=(weightn/sum(weightn,na.rm=T))-0.01 else weightn=weightn
#	wp=(1-sum(weightp,na.rm=T))/(n_pl-length(na.omit(weightp))); wpl=ifelse(is.na(weightp),wp,weightp)
#	wn=(1-sum(weightn,na.rm=T))/(n_nl-length(na.omit(weightn))); wnl=ifelse(is.na(weightn),wn,weightn)
#
	#multiplication of each layer by its weight
#	rst.nl.w=(rasn*wnl)
#	rst.pl.w=(rasp*wpl)
#
#	# sum of positive and negative layers in a final raster
#	rst.comb.w=calc(rst.pl.w,sum)+calc(rst.nl.w,sum)
#
#	return(rst.comb.w)
#}


#####################################################################################
#####################################################################################
#################### LWC CURRENT METHOD : 'lwc2' function ###########################
#####################################################################################
#####################################################################################


#' Linear Weight Combinaison of the criteria rasters
#' 
#' @param ras A rasterstack containing the criteria raster
#' @param weight A vector containing the weights for the criteria
#' @return the raster resulting of the LWC2 method
#' @examples
#' r1 <- raster(matrix(runif(100, min=-1, max=0),nrow=10, ncol=10))
#' r2 <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' ras <- stack(r1,r2)
#' w <- c(0.8,0.2)
#' lwc2(ras,w)
lwc2<-function(ras, weight){
  n_pnl <- length(x1p) + length(x1n)
  
  #normalization of the weights:  sum of positive criteria weights+sum of negative criteria weights=1
  #if (sum(weight,na.rm=T)>1 || sum(weight,na.rm=T)<1) {weight=(weight/sum(weight,na.rm=T))}
  if (sum(weight,na.rm=T)>1 || sum(weight,na.rm=T)<1) {weight=(weight/sum(weight,na.rm=T))}
  
  wl=ifelse(is.na(weight),0,weight)
  #multiplication of each layer by its weight
  rst.pnl.w=(ras*wl)
  
  # sum of each layer in a final raster
  rst.comb.w=calc(rst.pnl.w,sum)
  
  return(rst.comb.w)
}

#####################################################################################
#####################################################################################
#################### LWC-G METHOD : 'lwcg' function ###########################
#####################################################################################
#####################################################################################


#' Linear Weight Combinaison of the criteria rasters
#' 
#' @param ras A rasterstack containing the criteria raster
#' @param weight A vector containing the weights for the criteria
#' @param group A vector containing the number of the group for each criterion
#' @return the raster resulting of the LWC2 method
#' @examples
#' r1 <- raster(matrix(runif(100, min=-1, max=0),nrow=10, ncol=10))
#' r2 <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' ras <- stack(r1,r2)
#' w <- c(0.8,0.2)
#' lwc2(ras,w)
#lwc2<-function(ras, weight, group){
#  n_pnl <- length(x1p) + length(x1n)
  
  #normalization of the weights:  sum of positive criteria weights+sum of negative criteria weights=1
#  #if (sum(weight,na.rm=T)>1 || sum(weight,na.rm=T)<1) {weight=(weight/sum(weight,na.rm=T))}
#  if (sum(weight,na.rm=T)>1 || sum(weight,na.rm=T)<1) {weight=(weight/sum(weight,na.rm=T))}
  
#  wl=ifelse(is.na(weight),0,weight)
#  #multiplication of each layer by its weight
#  rst.pnl.w=(ras*wl)
  
  # sum of each layer in a final raster
#  rst.comb.w=calc(rst.pnl.w,sum)
  
#  return(rst.comb.w)
#}



##############################################################
################ 'AHP' function ##############################
##############################################################

#' Analytical Hierarchy Process (not used in the current application)
#' 
#' @param ras A rasterstack containing the criteria raster
#' @param mat A matrix containing the weights of the criteria compared to each other criteria
#' @return the raster resulting of the AHP 
#' @examples
#' r1 <- raster(matrix(runif(100, min=-1, max=0),nrow=10, ncol=10))
#' r2 <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' r3 <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' ras <- stack(r1,r2,r3)
#' m <- matrix(c(1,0.2,3,5,1,5,0.3,0.2,1), nrow=3, ncol=3, byrow = T)
#' ahp(ras,m)
ahp =function(ras, mat){					
	# Look for the eigenvalues (sorted by decreasing order) and eigenvectors of the matrix
	eig <-eigen(mat)
	# eigM = Mod(eig$values)
	# maxindex= which(eigM== max(eigM), arr.ind = TRUE)
	# weight=eig$vectors[, maxindex]
	weight <-eig$vectors[, 1]
	#Normalization
	weight<-weight/sum(weight)
	weight<-Mod(weight)
	#multiplication of each layer by its weight
	rst.pnl.w=(ras*weight)
	# sum of each layer in a final raster
	rst.comb.w=calc(rst.pnl.w,sum)
	#Consistency
	len<-length(weight)
	RI <-matrix(c(0.00, 0.00,0.58,0.90,1.12,1.24,1.32,1.41,1.49,1.51,1.48,1.56,1.57,1.59))
	CI <-(Mod(eig$values[1])-len)/(len-1)
	CR <-CI/RI[len]
	
	return(rst.comb.w)
}


###################################################################
################# 'calcthr' funtion ###############################
###################################################################


#' Create a dual raster with selected areas (pixels values > 0) and unselected ones(pixels values < 0) depending on the value of the threshold
#' 
#' @param ras A raster resulting from a method selected by the user
#' @param thr A threshold value
#' @return a dual raster. The pixels >0 are above the threshold, the pixels < 0 are below.
#' @examples
#' r <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' calcthr(r,0.5)
calcthr = function(ras,thr){
	rst.comb.q2 <- calc(ras, fun=function(x){ifelse(x<=thr,-0.5+x*x/100000,0.5-x*x/100000)}) 
	return(rst.comb.q2)
}

#############################################################
################ binarythr function #########################
#############################################################

#' Create a binary raster with selected areas (pixels values = 1) and unselected ones(pixels values = 0) depending on the value of the threshold
#' @param ras A raster resulting from a method selected by the user
#' @param thr A threshold value
#' @return a binary raster. The pixels with the value 1 are above the threshold, the pixels with 0 are below.
#' @examples
#' r <- raster(matrix(runif(100, min=0, max=1),nrow=10, ncol=10))
#' binarythr(r,0.5)

binarythr = function(etu,thr,quantile){
	r=calc(etu, fun=function(x){ifelse(x<=thr,ifelse(x==thr,2,0),1)})
	#spatial.point <-  SpatialPoints(coordinates(rst.comb.q22)[values(rst.comb.q22)==1 & !is.na(values(rst.comb.q22)),],proj4string = crs(rst.comb.q22),bbox =bbox(rst.comb.q22) )
	#rst.dist <- distanceFromPoints(object = rst.comb.q22, xy = spatial.point )
	#dist <- sort(unique(values(rst.dist)))
	#if (values(r[i])<dist[i+1] & r[i==2]){
  #rst.comb.q22[i] <- 1
  #}
	
	conditionPoint <- values(r)==2 & !is.na(values(r))
	conditionRef <- values(r)==1 & !is.na(values(r))
	coord.point <- coordinates(r)[conditionPoint,]
	coord.ref <- coordinates(r)[conditionRef,]
	spatial.point <- SpatialPoints(coords = coord.point, proj4string = crs(r), bbox = bbox(r))
	spatial.ref <- SpatialPoints(coords = coord.ref, proj4string = crs(r), bbox = bbox(r))
	length.spatial.point <- length(spatial.point)
	length.spatial.ref <- length(spatial.ref)
	numberPointSearched <- as.integer(quantile*(ncell(r)-length(r[is.na(r)])))
	numberPointNeed <- numberPointSearched - length.spatial.ref
	print(length.spatial.point)
	print(length.spatial.ref)
	if (length.spatial.point!=0 & length.spatial.ref!=0){
	quotient.point <-length.spatial.point %/% 600
	remainder.point <- length.spatial.point %% 600
	quotient.ref <- length.spatial.ref %/% 500
	remainder.ref <- length.spatial.ref %% 500 
	
	min.dist<-c(lapply(0:quotient.point,function(x){
	  miniMatrix(sapply(0:quotient.ref,function(y){
	    miniMatrix(pointDistance(spatial.point[600*x+(1:(ifelse(x==quotient.point,remainder.point,600)))],spatial.ref[500*y+(1:(ifelse(y==quotient.ref,remainder.ref,500)))]),ifelse(x==quotient.point,remainder.point,600), ifelse(y==quotient.ref,remainder.ref,500))
	  })
	  ,ifelse(x==quotient.point,remainder.point,600),quotient.ref+1) 
	})
	,recursive=T)
	
	tri.distance	<- sort(min.dist)
  aboveThr <- compare(coordinates(r), coord.point[min.dist < tri.distance[numberPointNeed],])
  values(r) <- ifelse(aboveThr, 1, ifelse(values(r)==2,0,values(r)))
	} else {
	  values(r)<- ifelse(values(r)==2,0, values(r))
	}
  
  return(r)

}
#if (length.spatial.point!=0){
 # min.dist <- sapply(1:length.spatial.point, function(x) {
  #  dist <- pointDistance(spatial.point[x],spatial.ref)
  #  mini(dist)
  #})
#conditionPoint <- values(r)==2 & !is.na(values(r))
#conditionRef <- values(r)==1 & !is.na(values(r))
#coord.point <- coordinates(r)[conditionPoint,]
#coord.ref <- coordinates(r)[conditionRef,]
#spatial.point <- SpatialPoints(coords = coord.point, proj4string = crs(r), bbox = bbox(r))
#spatial.ref <- SpatialPoints(coords = coord.ref, proj4string = crs(r), bbox = bbox(r))
#dist <- pointDistance(spatial.point,spatial.ref)
#length.spatial.point <- length(spatial.point)#
#length.spatial.ref <- length(spatial.ref)
#numberPointSearched <- as.integer(quantile*(ncell(r)-length(r[is.na(r)])))
#numberPointNeed <- numberPointSearched - length.spatial.ref
#print(c("length point et ref",length.spatial.point,length.spatial.ref))#

#if (length.spatial.point!=0){
#  min.dist<-mini(dist, length.spatial.point,length.spatial.ref)
#  tri.distance	<- sort(min.dist)
  
#  aboveThr <- compare(coordinates(r), coord.point[min.dist < tri.distance[numberPointNeed],])
 # values(r) <- ifelse(aboveThr, 1, ifelse(values(r)==2,0,values(r)))
#} else {
#  values(r)<- ifelse(values(r)==2,0, values(r))
#}

#' Normalize a raster between 0 and 1
#' @param ras A raster resulting from a method selected by the user
#' @return the raster normalized between 0 and 1.
#' @examples
#' r <- raster(matrix(runif(100, min=0, max=2),nrow=10, ncol=10))
#' normalizer(r)
normalizer = function(ras){
  	thrmin=as.numeric(ras@data@min)
  	thrmax=as.numeric(ras@data@max)
	rst.comb.w2=calc(ras, fun=function(x){ifelse((x>=thrmin),(x-thrmin)/(thrmax-thrmin),(x-thrmin)/(thrmax-thrmin))}) 
	return(rst.comb.w2)
}

#function to add labels in middle of tiles of pie chart 
text_pie = function(vector,labels=c()) {
  vector = vector/sum(vector)*2*pi
  temp = c()
  j = 0
  l = 0
  for (i in 1:length(vector)) {
    k = vector[i]/2        
    j =  j+l+k
    l = k
    text(cos(j)/2+(i-1)/15,sin(j)/2,labels[i])
  }
  vector = temp
}
