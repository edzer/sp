# loadMeuse = function(gridded = TRUE, river = FALSE) {
#    crs = CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
#    meuse = NULL
#    meuse.grid = NULL
#    meuse.riv = NULL
#    data("meuse", envir = environment())
#    coordinates(meuse) <- ~x+y
#    proj4string(meuse) <- crs
#    assign("meuse", meuse, envir = .GlobalEnv)
# 
#    data("meuse.grid", envir = environment())
#    if (gridded) {
#      gridded(meuse.grid) <- ~x+y
#    } else 
# 	 coordinates(meuse.grid) <- ~x+y
#    proj4string(meuse.grid) <- crs
#    assign("meuse.grid", meuse.grid, envir = .GlobalEnv)
# 
#    if (river) {
#      rm(meuse.riv)
#      data("meuse.riv", envir = environment())
#      meuse.riv <- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
#      proj4string(meuse.riv) <- crs
#      assign("meuse.riv", meuse.riv, envir = .GlobalEnv)
#    }
#    invisible(NULL)
# }

loadMeuse = function() {
	#.Deprecated("demo(meuse)")
	warning("please run demo(meuse) to load the meuse data set")
	demo("meuse")
}
