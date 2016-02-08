# Copyright (c) 2003-8 by Barry Rowlingson and Roger Bivand

if (!is.R()) {
  strsplit <- function(a,b) {
    if (a == as.character(NA))
        return(as.character(NA))
    else list(unlist(unpaste(a, b)))
  }
}

"CRS" <- function(projargs=NA_character_, doCheckCRSArgs=TRUE) {
# cautious change BDR 150424
    if (!is.na(projargs) && !nzchar(projargs)) projargs <- NA_character_
# condition added 140301
    stopifnot(is.logical(doCheckCRSArgs))
    stopifnot(length(doCheckCRSArgs) == 1L)
    stopifnot(is.character(projargs))
    if (!is.na(projargs)) {
        if (length(grep("^[ ]*\\+", projargs)) == 0)
            stop(paste("PROJ4 argument-value pairs must begin with +:", 
	        projargs))
    }
    if (!is.na(projargs)) {
        if (length(grep("latlon", projargs)) != 0)
            stop("northings must follow eastings: ", projargs)
        if (length(grep("lonlat", projargs)) != 0) {
            projargs <- sub("lon", "long", projargs)
            warning("'lonlat' changed to 'longlat': ", projargs)
        }
    }    
    if (is.na(projargs))
        uprojargs <- projargs
    else uprojargs <- paste(unique(unlist(strsplit(projargs, " "))), 
	collapse=" ")
    if (length(grep("= ", uprojargs)) != 0)
	stop(paste("No spaces permitted in PROJ4 argument-value pairs:", 
	    uprojargs))
    if (length(grep(" [:alnum:]", uprojargs)) != 0)
	stop(paste("PROJ4 argument-value pairs must begin with +:", 
	    uprojargs))
#    if (length(grep("rgdal", search()) > 0) &&
#      (sessionInfo()$otherPkgs$rgdal$Version > "0.4-2")) {
# sessionInfo()/read.dcf() problem in loop 080307
    if (doCheckCRSArgs) {
      if (!is.na(uprojargs) && requireNamespace("rgdal", quietly = TRUE)) {
        res <- rgdal::checkCRSArgs(uprojargs)
        if (!res[[1]]) 
            stop(res[[2]])
        uprojargs <- res[[2]]
      }
    }
    res <- new("CRS", projargs=uprojargs)
    res
}

"print.CRS" <- function(x, ...)
{
    pst <- paste(strwrap(x@projargs), collapse="\n")
    if (nchar(pst) < 40) cat(paste("CRS arguments:", pst, "\n"))
    else cat(paste("CRS arguments:\n", pst, "\n"))
    invisible(pst)
}

setMethod("show", "CRS", function(object) print.CRS(object))

identicalCRS = function(x, y) {
	if (! missing(y))
		identical(CRS(proj4string(x)), CRS(proj4string(y)))
	else { # x has to be list:
		stopifnot(is.list(x))
		if (length(x) > 1) {
			p1 = CRS(proj4string(x[[1]]))
			!any(!sapply(x[-1], function(p2) identical(CRS(proj4string(p2)), p1)))
		} else
			TRUE
	}
}
