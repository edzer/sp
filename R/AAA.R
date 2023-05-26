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
assign("evolution_status", 2L, envir=.spOptions) # default changed 2.0-0

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
# 1.5-1 DEFAULT: "none"
      rgdal_show_exportToProj4_warnings <- "none"
    }
  } else {
# 1.5-1 DEFAULT: "none"
    rgdal_show_exportToProj4_warnings <- "none"
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
    evolution_status <- as.integer(evolution_status)
    stopifnot(is.integer(evolution_status))
    stopifnot(evolution_status >= 0L && evolution_status <= 2L)
    assign("evolution_status", unname(evolution_status), envir=.spOptions)
  } else {
    evolution_status <- Sys.getenv("_SP_EVOLUTION_STATUS_")
    if (nchar(evolution_status) > 0L) {
      evolution_status <- as.integer(evolution_status)
      stopifnot(is.integer(evolution_status))
      stopifnot(evolution_status >= 0L && evolution_status <= 2L)
      assign("evolution_status", unname(evolution_status), envir=.spOptions)
    }
  }
  Smess <- paste("The legacy packages maptools, rgdal, and rgeos, underpinning this package\nwill retire shortly. Please refer to R-spatial evolution reports on\nhttps://r-spatial.org/r/2023/05/15/evolution4.html for details.\nThis package is now running under evolution status", get_evolution_status(), "\n")
  packageStartupMessage(Smess, appendLF = FALSE)
}

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
