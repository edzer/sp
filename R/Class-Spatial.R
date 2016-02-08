# Lancaster, Thu Nov  4 14:44:00 GMT 2004, fresh start from icelfloe
setClass("Spatial",
	slots = c(bbox = "matrix", proj4string = "CRS"),
	validity = function(object) {
# print("Entering validation: Spatial")
		bb = bbox(object)
		if (!is.matrix(bb))
			return("bbox should be a matrix")
		if (!mode(bb) == "numeric")
			return("bbox should be a numeric matrix")
		n = dimensions(object)
		if (n < 2)
			return("spatial.dimension should be 2 or more") 
		if (any(is.na(bb)))
			return("bbox should never contain NA values")
		if (any(!is.finite(bb)))
			return("bbox should never contain infinite values")
		if (any(bb[,"max"] < bb[,"min"]))
			return("invalid bbox: max < min")
		if (!is(object@proj4string, "CRS"))
			return("proj4string slot should be of class CRS")
		p4str <- object@proj4string@projargs
		if (!is.na(p4str) && !nzchar(p4str)) {
		  res <- grep("longlat", p4str, fixed=TRUE)
		  if (length(res) != 0) {# unprojected,
                    ll_sanity_res <- .ll_sanity(bb)
		    if (!ll_sanity_res) {
                      lst <- sapply(attr(ll_sanity_res, "details"),
                        attr, "out")
                      out <- paste(format(unlist(lst), digits=12), collapse=" ")
                      mess <- paste("Geographical CRS given to",
                        "non-conformant data:", out)
                      if (get_ll_warn())
		        warning(mess)
                      else
		        stop(mess)
                    }
                  }
# split out from proj4string<- and Spatial validity to cover numerical fuzz
# RSB 070216
#		    		&& any(bb[1,1] < -180 || bb[1,2] > 360 || 
#					bb[2,1] < -90 || bb[2,2] > 90))
#				return("Geographical CRS given to non-conformant data")
		}
		# validate proj4string here? -- no, that's rdgal's business
		return(TRUE)
	}
)
