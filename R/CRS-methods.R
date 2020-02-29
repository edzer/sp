# Copyright (c) 2003-8 by Barry Rowlingson and Roger Bivand

if (!is.R()) {
  strsplit <- function(a,b) {
    if (a == as.character(NA))
        return(as.character(NA))
    else list(unlist(unpaste(a, b)))
  }
}

"CRS" <- function(projargs=NA_character_, doCheckCRSArgs=TRUE,
    SRS_string=NULL) {
# cautious change BDR 150424
# trap NULL too 200225
    if (is.null(projargs))
        warning("CRS: projargs should not be NULL; set to NA")
    if ((is.null(projargs)) || (!is.na(projargs) && !nzchar(projargs))) projargs <- NA_character_
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
    if (is.na(projargs)) {
        uprojargs <- projargs
    } else {
        uprojargs <- paste(unique(unlist(strsplit(projargs, " "))), 
	collapse=" ")
        if (length(grep("= ", uprojargs)) != 0)
	    stop(paste("No spaces permitted in PROJ4 argument-value pairs:", 
	        uprojargs))
        if (length(grep(" [:alnum:]", uprojargs)) != 0)
	    stop(paste("PROJ4 argument-value pairs must begin with +:", 
	        uprojargs))
    }
#    if (length(grep("rgdal", search()) > 0) &&
#      (sessionInfo()$otherPkgs$rgdal$Version > "0.4-2")) {
# sessionInfo()/read.dcf() problem in loop 080307
    comm <- NULL
    if (!is.na(uprojargs) || !is.null(SRS_string)) {
        if (doCheckCRSArgs && requireNamespace("rgdal", quietly = TRUE)) {
            if ((length(grep("ob_tran", uprojargs)) > 0L) ||
                packageVersion("rgdal") < "1.5.1") {
                res <- rgdal::checkCRSArgs(uprojargs)
                if (!res[[1]]) stop(res[[2]])
                uprojargs <- res[[2]]
            } else if (packageVersion("rgdal") >= "1.5.1") {
                if (rgdal::new_proj_and_gdal()) {
                    res <- rgdal::checkCRSArgs_ng(uprojargs=uprojargs,
                        SRS_string=SRS_string)
                    if (!res[[1]]) stop(res[[2]])
                    uprojargs <- res[[2]]
                    comm <- res[[3]]
                } else { #stop("rgdal version mismatch")
                    res <- rgdal::checkCRSArgs(uprojargs)
                    if (!res[[1]]) stop(res[[2]])
                    uprojargs <- res[[2]]
                }
            } else stop("rgdal version mismatch")
        }
    }
    res <- new("CRS", projargs=uprojargs)
    if (!is.null(comm)) comment(res) <- comm
    res
}
if (!isGeneric("wkt"))
	setGeneric("wkt", function(obj)
		standardGeneric("wkt"))

setMethod("wkt", signature(obj = "CRS"),
	function(obj) {
                comm <- comment(obj)
                if (is.null(comm))
                    warning("CRS object has no comment")
		comm
        }
)


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
		identicalCRS1(CRS(proj4string(x)), CRS(proj4string(y)))
	else { # x has to be list:
		stopifnot(is.list(x))
		if (length(x) > 1) {
			p1 = CRS(proj4string(x[[1]]))
			!any(!sapply(x[-1], function(p2) identicalCRS1(CRS(proj4string(p2)), p1)))
		} else
			TRUE
	}
}

identicalCRS1 = function(x, y) {
  args_x <- strsplit(x@projargs, " +")[[1]]
  args_y <- strsplit(y@projargs, " +")[[1]]
  setequal(args_x, args_y)
}

is.na.CRS = function(x) {
	is.na(x@projargs)
}
