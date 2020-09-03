\name{sp}

\alias{sp}

\title{A package providing classes and methods for spatial data: points,
lines, polygons and grids }


\description{
This package provides S4 classes for importing, manipulating and exporting
spatial data in R, and for methods including print/show, plot, subset,
\code{[}, \code{[[}, \code{\$}, names, dim, summary, and a number of
methods specific to spatial data handling.

}%description


\section{Introduction}{
Several spatial statistical packages have been around for a long while,
but no organized set of classes for spatial data has yet been devised.
Many of the spatial packages make their own assumptions, or use their
own class definitions for spatial data, making it inconvenient to move
from one package to another. This package tries to provide a solid set
of classes for many different types of spatial data. The idea is that
spatial statistical packages will either support these classes (i.e.,
directly read and write them) or will provide conversion to them, so that
we have a base class set with which any package can exchange. This way,
many-to-many conversions can be replace with one-to-many conversions,
provided either in this package or the spatial packages. Wherever possible
conversion (coercion) functions are automatic, or provided by sp.

External packages that depend on sp will provide importing and exporting
from and to external GIS formats, e.g. through GDAL, OGR or shapelib.

In addition, this package tries to provide convenient methods to print,
summarize and plot such spatial data.

}%section{Introduction}

\section{Dimensions}{
In principal, geographical data are two-dimensional, on a flat surface (a
map) or on a sphere (the earth). This package provides space for dealing
with higher dimensional data where possible; this is e.g. very simple
for points and grids, but hard to do for polygons. Plotting functions
are devised primarily for two-dimensional data, or two-dimensional
projections of higher dimensional data.
}%section{Dimensions}

\section{Coordinate reference systems}{
Central to spatial data is that they have a coordinate reference system,
which is coded in object of CRS class. Central to operations on different
spatial data sets is that their coordinate reference system is compatible
(i.e., identical). 

This CRS can be a character string describing a reference system in
a way understood by the PROJ.4 projection library, or a (character)
missing value. An interface to the PROJ.4 library is available only if
the R package rgdal is present.
}

\section{Class structure}{
All spatial classes derive from a basic class \code{Spatial}, which only
provides a bounding box and a CRS. This class has no useful instances, but
useful derived classes. 

SpatialPoints extends Spatial and has coordinates.  The method
\link{coordinates} extracts the numeric matrix with coordinates from an
object of class SpatialPoints, or from other (possibly derived) classes
that have points.

Objects of class SpatialGrid points on a regular grid. Either a full grid
is stored or a partial grid (i.e., only the non-missing valued cells);
calling \link{coordinates} on them will give the coordinates for the
grid cells.

SpatialPoints, SpatialPixels and SpatialGrid can be of arbitrary
dimension, although most of the effort is in making them work for two
dimensional data.

SpatialLines provides lines, and SpatialPolygons provides polygons, i.e.,
lines that end where they start and do not intersect with itself.
SpatialLines and SpatialPolygons only have two-dimensional data.

SpatialPointsDataFrame extends SpatialPoints with a data slot, having
a data.frame with attribute data. Similarly, SpatialPixelsDataFrame,
SpatialLinesDataFrame, SpatialPolygonsDataFrame extend the primary
spatial information with attribute data.

}

\section{References}{
PROJ.4: \url{https://github.com/OSGeo/PROJ}

GDAL and OGR: \url{https://gdal.org/}.
}

\section{Authors}{
sp is a collaborative effort of Edzer Pebesma, Roger Bivand, Barry
Rowlingson and Virgilo G\'omez-Rubio.
}

\keyword{spatial}
