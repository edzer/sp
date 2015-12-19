\name{surfaceArea}
\alias{surfaceArea}
\alias{surfaceArea.matrix}
\alias{surfaceArea,SpatialPixelsDataFrame-method}
\alias{surfaceArea,SpatialGridDataFrame-method}
\alias{surfaceArea,matrix-method}

\title{
Compute surface area of a digital elevation model.
}
\description{
  It is often said that if Wales was flattened out it would have
  an area bigger than England. This function computes the surface
  area of a grid of heights taking into account the sloping nature
  of the surface.
}
\usage{
surfaceArea(m, ...)
surfaceArea.matrix(m, cellx = 1, celly = 1, byCell = FALSE)
}
\arguments{
  \item{m}{
    a matrix of height values, or an object of class \link{SpatialPixelsDataFrame} or \link{SpatialGridDataFrame}.
  }
  \item{cellx}{
    the size of the grid cells in the x-direction, in the same units as the height values.
  }
  \item{celly}{
    the size of the grid cells in the y-direction, in the same units as
    the height values.
}
  \item{byCell}{
    return single value or matrix of values
  }
  \item{...}{ignored}
}

\value{
  Either a single value of the total area if byCell=FALSE, or a matrix
  the same shape as m of individual cell surface areas if
  byCell=TRUE. In this case, the sum of the returned matrix should be
  the same value as that which is returned if byCell=FALSE.
  
  Missing values (NA) in the input matrix are allowed. They will produce
  an NA in the output matrix for byCell=TRUE, and contribute zero to the
  total area. They also have an effect on adjacent cells - see code
  comments for details.
  
}

\section{Methods}{
\describe{
\item{obj = "matrix"}{ takes a matrix as input, requires cellx and celly to be set }
\item{obj = "SpatialGridDataFrame"}{ takes an object of class \link{SpatialGridDataFrame} as input, and retrieves cellx and celly from this }
\item{obj = "SpatialPixelsDataFrame"}{ takes an object of class \link{SpatialPixelsDataFrame} as input, and retrieves cellx and celly from this }
}}


\references{
 Calculating Landscape Surface Area from Digital Elevation Models, Jeff S. Jenness
Wildlife Society Bulletin, Vol. 32, No. 3 (Autumn, 2004), pp. 829-839
}
\author{
Barry Rowlingson <b.rowlingson@lancaster.ac.uk>, integration in sp Edzer
Pebesma.
}

\examples{
surfaceArea(volcano)
image(surfaceArea(volcano,byCell=TRUE))

data(meuse.grid)
gridded(meuse.grid) = ~x+y
image(surfaceArea(meuse.grid["dist"], byCell=TRUE))
surfaceArea(meuse.grid["dist"])
}

\keyword{spatial}

