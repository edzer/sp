\name{mapasp}
\alias{mapasp}
\alias{degreeLabelsEW}
\alias{degreeLabelsNS}

\title{ Calculate aspect ratio for plotting geographic maps;
create nice degree axis labels }
\description{ Calculate aspect ratio for plotting geographic maps;
create nice degree axis labels }
\usage{ 
mapasp(data, xlim, ylim)
degreeLabelsEW(x)
degreeLabelsNS(x)
}
\arguments{
\item{data}{ object of class or extending \code{Spatial}}
\item{xlim}{ the xlim argument passed (or derived from bounding box) }
\item{ylim}{ the ylim argument passed (or derived from bounding box) }
\item{x}{ numeric; values at which tics and marks will be generated }
}
\value{
	\code{mapasp} is used for the aspect argument in lattice plots and spplot; 

	let x = dy/dx, with dy and dx the y- and x-size of the map.

	let s = 1/cos((My * pi)/180) with My the y coordinate of the middle of the map
	(the mean of ylim)

	for latlong (longlat) data, \code{mapasp} returns s * x.
	for other data, mapasp returns "iso".
}
\note{the values for \code{x} are typically obtained from \link{axTicks}}
\seealso{ \link{levelplot} in package \code{lattice}}

\keyword{dplot}
