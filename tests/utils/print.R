## print "NAs" as in R >= 4.6.0 to match reference output
if (getRversion() < "4.6.0") {
    print <- function (x) {
        if (inherits(x, "summary.Spatial"))
            x$data <- sub("NA's", "NAs ", x$data, fixed = TRUE)
        else if (is.table(x) && is.character(x))
            x <- sub("NA's", "NAs ", x, fixed = TRUE)
        base::print(x)
    }
}
