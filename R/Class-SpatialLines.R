setClass("Line", 
	slots = c(coords = "matrix"),
	validity = function(object) {
		if (any(is.na(object@coords)))
			stop("coords cannot contain missing values")
		if (ncol(object@coords) != 2)
			return("coords should have 2 columns")
#		if (nrow(object@coords) < 2)
#			return("Line should have at least 2 points")
		return(TRUE)
	}
)

setClass("Lines",
	slots = c(Lines = "list", ID = "character"),
	validity = function(object) {
		if (any(sapply(object@Lines, function(x) !is(x, "Line"))))
			stop("not a list of Line objects")
		return(TRUE)
})

setClass("SpatialLines",
	contains = "Spatial",
	slots = c(lines = "list"),
	validity = function(object) {
		if (any(unlist(lapply(object@lines, function(x) 
			!is(x, "Lines"))))) stop("lines not Lines objects")
                IDs <- sapply(slot(object, "lines"), function(i) slot(i, "ID"))
		if (anyDuplicated(IDs))
			return("non-unique Lines ID slot values")
#		if (length(object@lines) != 
#			length(unique(sapply(slot(object, "lines"),
#                            function(x) slot(x, "ID"))))) 
#				return("non-unique Lines ID slot values")
		return(TRUE)
	}
)

getSLlinesSlot <- function(SL) {
    .Deprecated("slot", msg="use *apply and slot directly", package = "sp")
    SL@lines
}

getLinesLinesSlot <- function(SL) {
    .Deprecated("slot", msg="use *apply and slot directly", package = "sp")
    SL@Lines
}

getLinesIDSlot <- function(Lines) {
    .Deprecated("slot", msg="use *apply and slot directly", package = "sp")
    Lines@ID
}

getSLLinesIDSlots <- function(SL) {
        .Deprecated("slot", msg="use *apply and slot directly", package = "sp")
	Sls <- slot(SL, "lines")
	sapply(Sls, function(x) slot(x, "ID"))
}
