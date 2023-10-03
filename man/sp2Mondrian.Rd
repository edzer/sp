\name{sp2Mondrian}
\alias{sp2Mondrian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{write map data for Mondrian}
\description{
  The function outputs a SpatialPolygonsDataFrame object to be used by Mondrian
}
\usage{
sp2Mondrian(SP, file, new_format=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SP}{a SpatialPolygonsDataFrame object}
  \item{file}{file where output is written}
  \item{new_format}{default TRUE, creates a text data file and a separate map file; the old format put both data sets in a single file - the map file is named by inserting "MAP_" into the file= argument after the rightmost directory separator (if any)}
}


\references{\url{https://www.theusrus.de/Mondrian/}, Ihaka Lecture 1: 28 September 2023, Interactive Graphics and Data Analysis, Antony Unwin \url{https://www.auckland.ac.nz/en/science/about-the-faculty/department-of-statistics/ihaka-lecture-series.html}}
\author{Patrick Hausmann and Roger Bivand}
\note{
 At this release, the function writes out a text file with both data and polygon(s) identified as belonging to each row of data.
}

\examples{
if (require("sf", quietly=TRUE)) {
td <- tempdir()
xx <- as(sf::st_read(system.file("shape/nc.shp", package="sf")[1], quiet=TRUE), "Spatial")
sp2Mondrian(xx, file=file.path(td, "sids1.txt"))
}
}
\keyword{spatial}
