# Copyright (c) 2003-20 by Barry Rowlingson and Roger Bivand

if (!is.R()) {
  strsplit <- function(a,b) {
    if (a == as.character(NA))
        return(as.character(NA))
    else list(unlist(unpaste(a, b)))
  }
}

if (!isGeneric("rebuild_CRS"))
	setGeneric("rebuild_CRS", function(obj)
		standardGeneric("rebuild_CRS"))

setMethod("rebuild_CRS", signature(obj = "CRS"),
	function(obj) {
            if (is.null(comment(obj))) {
                obj <- CRS(slot(obj, "projargs"))
            } 
            obj
        }
)


"CRS" <- function(projargs=NA_character_, doCheckCRSArgs=TRUE,
    SRS_string=NULL, get_source_if_boundcrs=TRUE) {
# cautious change BDR 150424
# trap NULL too 200225
    if (is.null(projargs))
        warning("CRS: projargs should not be NULL; set to NA")
    if ((is.null(projargs)) || (!is.na(projargs) && !nzchar(projargs))) projargs <- NA_character_
# condition added 140301
    stopifnot(is.logical(doCheckCRSArgs))
    stopifnot(length(doCheckCRSArgs) == 1L)
    stopifnot(is.logical(get_source_if_boundcrs))
    stopifnot(length(get_source_if_boundcrs) == 1L)
    stopifnot(is.character(projargs))
    if (!is.na(projargs)) {
        if (length(grep("^[ ]*\\+", projargs)) == 0) {
            if (is.null(SRS_string)) {
                if (doCheckCRSArgs && 
                    requireNamespace("rgdal", quietly = TRUE)) {
                    if (packageVersion("rgdal") >= "1.5.1" && 
                        rgdal::new_proj_and_gdal()) {
                        SRS_string <- projargs
                        projargs <- NA_character_
                    }
                }
            } else {
                stop(paste("PROJ4 argument-value pairs must begin with +:", 
	            projargs))
            }
        }
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
            if (packageVersion("rgdal") < "1.5.1") {
                res <- rgdal::checkCRSArgs(uprojargs)
                if (!res[[1]]) stop(res[[2]])
                uprojargs <- res[[2]]
            } else if (packageVersion("rgdal") >= "1.5.1") {
                if (rgdal::new_proj_and_gdal()) {
                    if (packageVersion("rgdal") >= "1.5.17") {
                        res <- rgdal::checkCRSArgs_ng(uprojargs=uprojargs,
                            SRS_string=SRS_string,
                            get_source_if_boundcrs=get_source_if_boundcrs)
                    } else {
                        res <- rgdal::checkCRSArgs_ng(uprojargs=uprojargs,
                            SRS_string=SRS_string)
                    }
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
                if (is.null(comm)) {
                  if (get("rgdal_show_exportToProj4_warnings",
                    envir=.spOptions)) {
                    if (!get("thin_PROJ6_warnings", envir=.spOptions)) {
                      warning("CRS object has no comment")
                    } else {
                      if (get("PROJ6_warnings_count",
                        envir=.spOptions) == 0L) {
                        warning("CRS object has no comment\n repeated warnings suppressed")
                      }
                      assign("PROJ6_warnings_count",
                        get("PROJ6_warnings_count",
                        envir=.spOptions) + 1L, envir=.spOptions)
                   }
                 }
                }
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
	if (! missing(y)) {
            if (inherits(x, "ST")) x <- slot(slot(x, "sp"), "proj4string")
            else if (inherits(x, "Raster")) x <- slot(x, "crs")
            else x <- slot(x, "proj4string")
            if (inherits(y, "ST")) y <- slot(slot(y, "sp"), "proj4string")
            else if (inherits(y, "Raster")) y <- slot(y, "crs")
            else y <- slot(y, "proj4string")
	    identicalCRS1(rebuild_CRS(x), rebuild_CRS(y))
	} else { # x has to be list:
		stopifnot(is.list(x))
                if (inherits(x[[1]], "Tracks")) {
                    x <- unlist(lapply(x, function(j) {
                        y <- slot(j, "tracks") 
                          if (!is.null(y)) lapply(y, function(l) 
                            if (!is.null(l)) slot(l, "sp"))}))
                }
		if (length(x) > 1) {
                    if (inherits(x[[1]], "ST")) 
                        x[[1]] <- slot(slot(x[[1]], "sp"), "proj4string")
                    else if (inherits(x[[1]], "Raster"))
                        x[[1]] <- slot(x[[1]], "crs")
                    else x[[1]] <- slot(x[[1]], "proj4string")
		    p1 = rebuild_CRS(x[[1]])
		    !any(!sapply(x[-1], function(p2) {
                        if (inherits(p2, "ST")) 
                            p2 <- slot(slot(p2, "sp"), "proj4string")
                        else if (inherits(p2, "Raster")) p2 <- slot(p2, "crs")
                        else p2 <- slot(p2, "proj4string")
                        identicalCRS1(rebuild_CRS(p2), p1)}))
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
