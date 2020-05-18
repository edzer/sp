# Copyright (c) 2003-7 by Barry Rowlingson and Roger Bivand

setClass("CRS", slots = c(projargs = "character"),
# changed to NA_character_ RSB 2020-02-28
	prototype = list(projargs = NA_character_),
	validity = function(object) {
		if (length(object@projargs) != 1)
			return("projargs must be of length 1")
	}
)
