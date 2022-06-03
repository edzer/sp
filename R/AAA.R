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
assign("evolution_status", 0L, envir=.spOptions)

#.sp_CRS_cache <- new.env(FALSE, globalenv())
#assign("CRS_CACHE", list(), envir=.sp_CRS_cache)
.sp_CRS_cache <- new.env(hash=TRUE, globalenv())

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
  evolution_status <- unlist(options("sp_evolution_status"))
  if (!is.null(evolution_status)) {
    stopifnot(is.integer(as.integer(evolution_status)))
    stopifnot(evolution_status >= 0L && evolution_status <= 2L)
    assign("evolution_status", unname(evolution_status), envir=.spOptions)
  } else {
    evolution_status <- Sys.getenv("_SP_EVOLUTION_STATUS_")
    if (nchar(evolution_status) > 0L) {
      stopifnot(is.integer(as.integer(evolution_status)))
      stopifnot(evolution_status >= 0L && evolution_status <= 2L)
      assign("evolution_status", unname(evolution_status), envir=.spOptions)
    }
  }
}

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
