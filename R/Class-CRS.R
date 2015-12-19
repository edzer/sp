# Copyright (c) 2003-7 by Barry Rowlingson and Roger Bivand

setClass("CRS", slots = c(projargs = "character"),
	prototype = list(projargs = character(1)),
	validity = function(object) {
		if (length(object@projargs) != 1)
			return("projargs must be of length 1")
	}
)
