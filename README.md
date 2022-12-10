# sp

[![R-CMD-check](https://github.com/edzer/sp/workflows/tic/badge.svg)](https://github.com/edzer/sp/actions)
[![cran checks](https://badges.cranchecks.info/badges/worst/sp.svg)](https://cran.r-project.org/web/checks/check_results_sp.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/sp?color=brightgreen)](http://www.r-pkg.org/pkg/sp)

R Classes and Methods for Spatial Data.

*Before* **sp** is loaded, `options("sp_evolution_status")` may be set to `0L` for legacy behaviour, `1L` for setting `CRS(, doCheckCRSArgs=FALSE, )` and other minor steps to block calls out to **rgdal** etc., and `2L` to use **sf** internally (not completed yet). If this option is not set, the environment variable `_SP_EVOLUTION_STATUS_` can be used, again *before* **sp** is loaded, set to `"0"`, `"1"` or `"2"`; this is used for running revdep checks under different evolution scenarios, anticipating [retirement of maptools, rgdal and rgeos](https://r-spatial.org/r/2022/04/12/evolution.html).

See:

* Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial data in R. 
[R News 5 (2)](https://cran.r-project.org/doc/Rnews/Rnews_2005-2.pdf).
* Roger S. Bivand, Edzer Pebesma, Virgilio Gómez-Rubio, 2013. Applied Spatial Data 
Analysis with R, [Second edition](https://www.asdar-book.org/). Springer, NY.  
* an sp [maps gallery](https://edzer.github.io/sp).
