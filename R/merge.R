# Author: Robert J. Hijmans
# Date : November 2011 / October 2015
# Version 2
# Licence GPL v3

if (!isGeneric("merge")) {
    setGeneric("merge", function(x, y, ...)
        standardGeneric("merge"))
}


setMethod('merge', signature(x='Spatial', y='ANY'), 
  function(x, y, ...) {
	y <- try(as.data.frame(y))
	if (class(y) != 'data.frame') {
		stop('y cannot be coerced to a data.frame')
	}
	merge(x, y)
}
)


setMethod('merge', signature(x='Spatial', y='data.frame'), 
  function(x, y, by=intersect(names(x), names(y)), by.x=by, 
		by.y=by, all.x=TRUE, suffixes = c(".x",".y"), 
		incomparables = NULL, duplicateGeoms=FALSE, ...) {

	if (!('data' %in% slotNames(x)))
		stop('x has no attributes')

		
	x$DoNotUse_temp_sequential_ID_963 <- 1:nrow(x)
	d <- merge(x@data, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, 
		incomparables=incomparables, all.x=all.x, all.y=FALSE)

    if (!all.x) {
		# Spatial* objects cannot have NULL geometries
		if (nrow(d) == 0) {
			warning('no matching records')
			return(NULL)
		}
	}

	# sort the merged table
	d <- d[order(d$DoNotUse_temp_sequential_ID_963), ]
	
	# Normally we want one-to-one joins with spatial data
	if (!duplicateGeoms) {
		if (any(table(d$DoNotUse_temp_sequential_ID_963) > 1)) {
			stop('non-unique matches detected')
		}
	} 
	
	# duplicate (duplicateGeoms = TRUE) or remove (all.x=FALSE) records if needed
	x <- x[d$DoNotUse_temp_sequential_ID_963, ]
		
	d$DoNotUse_temp_sequential_ID_963 <- NULL
	x@data <- d
	x
}
)


		
		