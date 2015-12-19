\name{Line}
\alias{Line}
\alias{Lines}
\title{ create objects of class Line or Lines}
\description{ create objects of class \code{Line} or \code{Lines} from coordinates}
\usage{ 
Line(coords)
Lines(slinelist, ID)
}
\arguments{
\item{coords}{ 2-column numeric matrix with coordinates for a single line }
\item{slinelist}{ list with elements of class \link{Line-class}}
\item{ID}{a single word unique character identifier, 
character vector of length one}
}
\value{
	\code{Line} returns an object of class \link{Line-class};
	\code{Lines} returns an object of class \link{Lines-class}
}
\seealso{ \link{SpatialLines-class} }
\examples{ 
# from the sp vignette:
l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
}
\keyword{classes}
