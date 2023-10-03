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
assign("evolution_status", 2L, envir=.spOptions) # fixed changed 2.1-0
assign("startup_message", "none", envir=.spOptions) # https://github.com/serafinialessio/mapping/issues/3

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
  Smess <- paste0("The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,\nwhich was just ", where, "ed, were retired in October 2023.\nPlease refer to R-spatial evolution reports for details, especially\nhttps://r-spatial.org/r/2023/05/15/evolution4.html.\nIt may be desirable to make the sf package available;\npackage maintainers should consider adding sf to Suggests:.\n")
  packageStartupMessage(Smess, appendLF = FALSE)
}

.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
