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
.onUnload <- function(libpath)
    library.dynam.unload("sp", libpath)
