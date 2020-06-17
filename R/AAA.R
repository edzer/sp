#.onLoad <- function(lib, pkg) {
# 	require(methods)
#}
.spOptions <- new.env(FALSE, globalenv())
assign("ll_warn", FALSE, envir = .spOptions)
assign("ll_TOL", 0.25, envir = .spOptions)
assign("ReplCRS_warn", TRUE, envir = .spOptions)
assign("Polypath", TRUE, envir = .spOptions)
assign("PolypathRule", "winding", envir = .spOptions)
assign("col.regions", bpy.colors(), envir = .spOptions)
assign("thin_PROJ6_warnings", FALSE, envir=.spOptions)
assign("PROJ6_warnings_count", 0L, envir=.spOptions)

.onLoad <- function(lib, pkg) {
  load_stuff()
}

load_stuff <- function() {
  rgdal_show_exportToProj4_warnings <- options("rgdal_show_exportToProj4_warnings")
  if (!is.null(rgdal_show_exportToProj4_warnings)) {
    if (!(rgdal_show_exportToProj4_warnings %in% c("all", "thin", "none"))) {
# CURRENT DEFAULT: "all"
      rgdal_show_exportToProj4_warnings <- "all"
    }
  } else {
# CURRENT DEFAULT: "all"
    rgdal_show_exportToProj4_warnings <- "all"
  }
  if (rgdal_show_exportToProj4_warnings == "all") {
    assign("rgdal_show_exportToProj4_warnings", TRUE, envir=.spOptions)
  } else if (rgdal_show_exportToProj4_warnings == "thin") {
    assign("rgdal_show_exportToProj4_warnings", TRUE, envir=.spOptions)
    assign("thin_PROJ6_warnings", TRUE, envir=.spOptions)
  } else {
    assign("rgdal_show_exportToProj4_warnings", FALSE, envir=.spOptions)
  }
}

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
