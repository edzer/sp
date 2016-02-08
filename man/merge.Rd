\name{merge}

\docType{methods}

\alias{merge}
\alias{merge,Spatial,data.frame-method}
\alias{merge,Spatial,ANY-method}

\title{ 
Merge a Spatial* object having attributes with a data.frame
}

\description{
Merge a Spatial object having a data.frame (i.e. merging of non-spatial attributes).
}

\usage{
\S4method{merge}{Spatial,data.frame}(x, y, by = intersect(names(x), names(y)),
 by.x = by, by.y = by, all.x = TRUE, suffixes = c(".x",".y"),
 incomparables = NULL, duplicateGeoms = FALSE, ...)
}

\arguments{
  \item{x}{object deriving from \link{Spatial}}
  \item{y}{object of class \code{data.frame}, or any other class that can be coerced to a data.frame with \code{as.data.frame}}
  \item{by, by.x, by.y}{specifications of the common columns. See 'Details' in (base) \code{\link[base]{merge}}.}
  \item{all.x}{logical; if TRUE, then the returned object will have all rows of x, even those that has no matching row in y. These rows will have NAs in those columns that are usually filled with values from y}
  \item{suffixes}{character(2) specifying the suffixes to be used for making non-by names() unique.}
  \item{incomparables}{values which cannot be matched. See \code{\link[base]{match}}.}
  \item{duplicateGeoms}{logical; if \code{TRUE} geometries in \code{x} are
  duplicated if there are multiple matches between records in \code{x}
  and \code{y}}
  \item{...}{arguments to be passed to or from methods.}
} 

\value{ a Spatial* object }

\author{Robert J. Hijmans}

\seealso{ \code{\link[base]{merge}} }

\keyword{methods}
\keyword{spatial}

