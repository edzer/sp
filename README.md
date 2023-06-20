# sp

[![R-CMD-check](https://github.com/edzer/sp/workflows/tic/badge.svg)](https://github.com/edzer/sp/actions)
[![cran checks](https://badges.cranchecks.info/badges/worst/sp.svg)](https://cran.r-project.org/web/checks/check_results_sp.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/sp?color=brightgreen)](http://www.r-pkg.org/pkg/sp)

R Classes and Methods for Spatial Data.

From version 2.0.0, `sp` evolution status is changed to `2L` to use **sf** internally in place of `rgdal`, from default `0L` until now. See [this report](https://r-spatial.org/r/2023/05/15/evolution4.html) for adaptation details, and [this report](https://r-spatial.org/r/2023/04/10/evolution3.html) for progress on retiring `maptools`, `rgdal` and `rgeos`.

*Before* **sp** is loaded, `options("sp_evolution_status")` may be set to `0L` for legacy behaviour, `1L` for setting `CRS(, doCheckCRSArgs=FALSE, )` and other minor steps to block calls out to **rgdal** etc., and `2L` to use **sf** internally. If this option is not set, the environment variable `_SP_EVOLUTION_STATUS_` can be used, again *before* **sp** is loaded, set to `"0"`, `"1"` or `"2"`; this is used for running revdep checks under different evolution scenarios, anticipating [retirement of maptools, rgdal and rgeos](https://r-spatial.org/r/2022/04/12/evolution.html).

From `sp 1.6.0` published on CRAN 2023-01-19, these status settings may also be changed when `sp` is loaded, using `sp::get_evolution_status()` returning the current value, and `sp::set_evolution_status(value)`, where value can take the integer values `0L`, `1L` and `2L`. `sp 1.6.1` published on CRAN 2023-05-25 added start-up messages to alert users to imminent changes.

See:

* Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial data in R. 
[R News 5 (2)](https://cran.r-project.org/doc/Rnews/Rnews_2005-2.pdf).
* Roger S. Bivand, Edzer Pebesma, Virgilio Gómez-Rubio, 2013. Applied Spatial Data 
Analysis with R, [Second edition](https://www.asdar-book.org/). Springer, NY.  
* an sp [maps gallery](https://edzer.github.io/sp).
