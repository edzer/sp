setClass("DMS", 
	#slots = c(WS="logical", deg="numeric", min="numeric", sec="numeric", NS="logical"),
	representation(WS="logical", deg="numeric", min="numeric", sec="numeric", NS="logical"),
	validity = function(object) {
	    if (object@NS) {
		if (any(abs(object@deg) > 90)) return("abs(degree) > 90")
	    } else {
	        if (any(abs(object@deg) > 360))
	            return("abs(degree) > 360")
	        else if (any(object@WS & (object@deg > 180)))
		    return("degree < -180")
                else return(TRUE)
	    }
	}
)

"dd2dms" <- function(dd, NS=FALSE) {
	sdd <- sign(dd)
	WS <- ifelse(sdd < 0, TRUE, FALSE) 
	dd <- abs(dd)
	deg <- as(floor(dd), "integer")
	dd <- (dd - deg)*60
	mins <- as(floor(dd), "integer")
	sec <- (dd - mins)*60
	tst <- abs(sec - 60.0) > sqrt(.Machine$double.eps)
        sec <- ifelse(tst, sec, 0.0)
	mins <- ifelse(tst, mins, mins+1)
	tst <- mins < 60
        mins <- ifelse(tst, mins, 0)
	deg <- ifelse(tst, deg, deg+1)

	dms <- new("DMS", WS=WS, deg=deg, min=mins, sec=sec, NS=NS)
	tst <- validObject(dms)
	if (is.logical(tst) & tst) return(dms)
	else stop(tst)
	dms
}

as.double.DMS <- function(x, ...) {
	dd <- x@deg + x@min/60 + x@sec/3600
	dd <- ifelse(x@WS, -dd, dd)
	dd
}

as.numeric.DMS <- function(x, ...) {
	if (!inherits(x, "DMS")) stop("not a DMS object")
	as.double.DMS(x)
}

setAs("DMS", "numeric", function(from) as.numeric.DMS(from))

as.character.DMS <- function(x, ...) {
	if (!inherits(x, "DMS")) stop("not a DMS object")
	if (!x@NS) tag <- c("W", "E")
	else tag <- c("S", "N")
	res <- ifelse(x@WS, tag[1], tag[2])
	res <- paste(ifelse(round(x@sec, digits=3) != "0", 
		paste(round(x@sec, digits=3), '\"', sep=""), ""), 
		res, sep="")
	res <- paste(ifelse(((x@min != 0) | 
		(round(x@sec, digits=3) != "0")),
		paste("d", x@min, "\'", sep=""), ""), res, sep="")
	res <- paste(x@deg, res, sep="")
	invisible(res)
}
setAs("DMS", "character", function(from) as.character.DMS(from))

"print.DMS" <- function(x, ...)
{
	res <- as(x, "character")
	print(res, quote=FALSE)
	invisible(res)
}
setMethod("show", "DMS", function(object) print.DMS(object))

"char2dms" <- function(from, chd="d", chm="'", chs='"') {
	x <- substr(from, nchar(from), nchar(from))
	NS <- any(x == "N" | x == "S")
	y <- substr(from, 1, nchar(from)-1)
	ndeg <- regexpr(chd, y)
	nmin <- regexpr(chm, y)
	nsec <- regexpr(chs, y)
	deg <- as(substr(y, 1, ndeg-1), "integer")
	smin <- substr(y, ndeg+1, nmin-1)
	dotmin <- regexpr("\\.", smin)
	ifelse (dotmin < 0, {
		mins <- as(ifelse(nmin < 1, 0, smin), "integer")
		sec <- as(ifelse(nsec < 1, 0, 
		    substr(y, nmin+1, nsec-1)), "numeric")
	}, {
		mins <- as(ifelse(nmin < 1, 0, smin), "integer")
		sec <- (as(smin, "numeric") - mins) * 60
	})
	WS <- ifelse(x == "W" | x == "S", TRUE, FALSE)
	    
	dms <- new("DMS", WS=WS, deg=deg, min=mins, sec=sec, NS=NS)
	tst <- validObject(dms)
	if (is.logical(tst) & tst) return(dms)
	else stop(tst)
	dms
}
