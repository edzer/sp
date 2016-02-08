get_ll_warn <- function() {
    get("ll_warn", envir = .spOptions)
}

get_ll_TOL <- function() {
    get("ll_TOL", envir = .spOptions)
}


get_ReplCRS_warn <- function() {
    get("ReplCRS_warn", envir = .spOptions)
}


set_ll_warn <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        assign("ll_warn", value, envir = .spOptions)
        get_ll_warn()
}

set_ll_TOL <- function(value) {
        stopifnot(is.numeric(value))
        stopifnot(length(value) == 1)
        stopifnot(value > 0)
        assign("ll_TOL", value, envir = .spOptions)
        get_ll_TOL()
}

set_ReplCRS_warn <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        assign("ReplCRS_warn", value, envir = .spOptions)
        get_ReplCRS_warn()
}

get_Polypath <- function() {
    get("Polypath", envir = .spOptions)
}

set_Polypath <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        assign("Polypath", value, envir = .spOptions)
        get_Polypath()
}

get_PolypathRule <- function() {
    get("PolypathRule", envir = .spOptions)
}

set_PolypathRule <- function(value) {
        stopifnot(is.character(value))
        stopifnot(length(value) == 1)
        stopifnot(value %in% c("winding", "evenodd"))
        assign("PolypathRule", value, envir = .spOptions)
        get_PolypathRule()
}

set_col_regions <- function(value) {
        stopifnot(is.character(value))
        stopifnot(length(value) > 1)
	    assign("col.regions", value, envir = .spOptions)
		get_col_regions()
}

get_col_regions <- function() {
	    get("col.regions", envir = .spOptions)
}
