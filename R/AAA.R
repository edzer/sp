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
assign("startup_message", "load", envir=.spOptions) 

#.sp_CRS_cache <- new.env(FALSE, globalenv())
#assign("CRS_CACHE", list(), envir=.sp_CRS_cache)
.sp_CRS_cache <- new.env(hash=TRUE, globalenv())

.onLoad <- function(lib, pkg) {
  load_stuff()
  if (get("startup_message", envir=.spOptions) == "load")
    smess_func()
}

.onAttach <- function(lib, pkg) {
  if (get("startup_message", envir=.spOptions) == "attach")
    smess_func()
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
  startup_message <- unlist(options("sp_startup_message"))
  if (!is.null(startup_message)) {
    startup_message <- as.character(startup_message)
    stopifnot(is.character(startup_message))
    stopifnot(isTRUE(startup_message %in% c("attach", "load", "none")))
    assign("startup_message", unname(startup_message), envir=.spOptions)
  } else {
    startup_message <- Sys.getenv("_SP_STARTUP_MESSAGE_")
    if (nchar(startup_message) > 0L) {
      startup_message <- as.character(startup_message)
      stopifnot(is.character(startup_message))
      stopifnot(isTRUE(startup_message %in% c("attach", "load", "none")))
      assign("startup_message", unname(startup_message), envir=.spOptions)
    }
  }
}

smess_func <- function() {
  where <- get("startup_message", envir=.spOptions)
  Smess <- paste("The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,\nwhich was just ", where, "ed, will retire in October 2023.\nPlease refer to R-spatial evolution reports for details, especially\nhttps://r-spatial.org/r/2023/05/15/evolution4.html.\nThe sp package is now running under evolution status ", get_evolution_status(), "\n", ifelse(get_evolution_status() == 2L, "     (status 2 uses the sf package in place of rgdal)\n", ""), sep="")
  packageStartupMessage(Smess, appendLF = FALSE)
}

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
