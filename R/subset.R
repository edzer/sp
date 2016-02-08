# taken from subset.data.frame:
subset.Spatial = function(x, subset, select, drop = FALSE, ...) {

	if (! "data" %in% slotNames(x))
		stop("subset only works for Spatial*DataFrame objects")
    if (missing(subset))
        r <- TRUE
    else {
# outcomment, suggested by Sebastian Meyer, 06/26/2012:
#		if (is.logical(subset) && missing(select))
#			return(x[subset & !is.na(subset),])
        e <- substitute(subset)
        r <- eval(e, x@data, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must be or evaluate to logical")
        r <- r & !is.na(r)
    }
   	if (missing(select)) 
       	vars <- TRUE
   	else {
       	nl <- as.list(seq_along(x@data))
       	names(nl) <- names(x@data)
       	vars <- eval(substitute(select), nl, parent.frame())
   	}
	x[r, vars, drop = drop]
}
