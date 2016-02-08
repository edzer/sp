surfaceArea.matrix <- function(m,cellx=1,celly=1,byCell=FALSE){

  if(byCell){
    mout = matrix(NA,nrow=nrow(m),ncol=ncol(m))
  }else{
    mout = NA
  }

  m=cbind(m[,1],m,m[,ncol(m)])
  m=rbind(m[1,],m,m[nrow(m),])
  storage.mode(m) <- "double"
  
  ret = .C("sarea",
    m,
    as.integer(nrow(m)),
    as.integer(ncol(m)),
    as.double(cellx),
    as.double(celly),
    sa=as.double(mout),
    bycell=as.integer(byCell),
    NAOK=TRUE)
  if(!byCell){
    return(ret$sa)
  }else{
    return(matrix(ret$sa,ncol=ncol(mout),nrow=nrow(mout)))
  }
} 
setMethod("surfaceArea", signature("matrix"), surfaceArea.matrix)

surfaceArea.SpatialGridDataFrame <- function(m,cellx=1,celly=1,byCell=FALSE){
	cs = gridparameters(m)$cellsize
	ret = surfaceArea(as.matrix(m), cs[1], cs[2], byCell = byCell)
	if (is.matrix(ret)) {
		m[[1]] = as.vector(ret)
		m
	} else
		ret
}
setMethod("surfaceArea", signature("SpatialGridDataFrame"), 
	surfaceArea.SpatialGridDataFrame)

surfaceArea.SpatialPixelsDataFrame <- function(m, byCell=FALSE)
	surfaceArea(as(m, "SpatialGridDataFrame"), byCell = byCell)
setMethod("surfaceArea", signature("SpatialPixelsDataFrame"), 
	surfaceArea.SpatialPixelsDataFrame)
