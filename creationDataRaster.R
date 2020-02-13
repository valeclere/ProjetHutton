library(raster)

#' Create the hardconstraints raster
#' 
#' @param datatable A datatable containing the hardconstraints files name
#' @return the rasterstack containing the binary hard constraints rasters. The hards constraints match the pixels whose value are -1.
#' @examples
#' HC <-read.table(paste0("../data/Hard_Constraint.txt"),h=T)
#' creationHardConstraintRaster(HC)
creationHardConstraintRaster <- function(datatable){
	#Creation of the hard constraint raster 
  rst.hcl=stack()
  n_hcl=max(length(datatable[,1]),invert=T)
  
  Q = sapply(1:n_hcl, function(i) {
    rst<<-raster(readGDAL(toString(paste0("../data/Hard_Constraint/",datatable[i,2]))))
    rst[is.na(rst)]=0
    rst.hcl=addLayer(rst.hcl,rst)
  }
  )
  rst.hcl=stack(Q)
  names(rst.hcl) <-c(sapply(datatable[,3], toString))
  return(rst.hcl)
}

#' Create the hardconstraints raster uploaded by the user
#' 
#' @param datatable A datatable containing the hardconstraints files name
#' @param nameuploaded Name of the folder uploaded
#' @return the rasterstack containing the binary hard constraints rasters. The hards constraints match the pixels whose value are -1.
#' @examples
#' HC <-read.table(paste0(file.path(file.path("../data", nameuploaded),"Hard_Constraint.txt"),h=T))
#' creationHardConstraintRaster(HC, nameuploaded)
creationHardConstraintRasterUser <- function(datatable, nameuploaded){
  #Creation of the hard constraint raster 
  rst.hcl=stack()
  n_hcl=max(length(datatable[,1]),invert=T)
  
  Q = sapply(1:n_hcl, function(i) {
    print(nameuploaded)
    rst<<-raster(readGDAL(toString(paste0(file.path(file.path("../data", nameuploaded),datatable[i,2])))))
    rst[is.na(rst)]=0
    rst.hcl=addLayer(rst.hcl,rst)
  }
  )
  rst.hcl=stack(Q)
  names(rst.hcl) <-c(sapply(datatable[,3], toString))
  return(rst.hcl)
}

#' Create the criteria raster
#' 
#' @param datatable A datatable containing the criteria files name
#' @return the rasterstack containing the criteria rasters.
#' @examples
#' Prinfo <- Pr[1,]
#' HP <- read.table(paste0("../data/Forest/", Prinfo[1,5]),h=T)
#' creationCriteriaRaster(HP)
creationCriteriaRaster <- function(datatable){
	#Number of criteria
	n=max(length(datatable[,1]),invert=T)

	#Names of criteria
	nm=datatable[,3]

	#Creation of the raster 
	rst.original=raster()

	#Loop to add layers to the positive raster, one layer per criteria
  Q = lapply(1:n, function(i) {
	  rst<<-raster(readGDAL(toString(paste0("../data/",type,"/",datatable[i,2]))))
	  names(rst)=nm[i]
	  rst.original=addLayer(rst.original,rst)
	}
	)
	rst.original=stack(Q)
	return(rst.original)
}

#' Bind the positive criteria raster and the negative criteria raster
#' 
#' @param rasp A positive criteria rasterstack
#' @param rasn A negative criteria rasterstack
#' @return the rasterstack containing the criteria rasters.
#' @examples
#' creationPNLCriteriaraster(creationCriteriaRaster(HP),creationCriteriaRaster(HN))
creationPNLCriteriaraster <- function(rasp, rasn){
	rst.pnl.original <- stack(rasp, rasn)
	return(rst.pnl.original)
}

