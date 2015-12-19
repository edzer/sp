setClass("GridTopology", 
	slots = c(cellcentre.offset = "numeric",
		cellsize = "numeric",
		cells.dim = "integer"),
	validity = function(object) {
		n = length(na.omit(object@cellcentre.offset))
		if (length(na.omit(object@cellsize)) != n)
			return("cellsize has incorrect dimension")
		if (sum(object@cells.dim > 0) != n)
			return("cells.dim has incorrect dimension")
		return(TRUE)
	}
)
