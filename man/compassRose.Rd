\name{compassRose}
\alias{compassRose}
\title{Display a compass rose.}
\description{Displays a basic compass rose, usually to orient a map.}
\usage{
 compassRose(x,y,rot=0,cex=1)
}
\arguments{
 \item{x,y}{The position of the center of the compass rose in user units.}
 \item{rot}{Rotation for the compass rose in degrees. See Details.}
 \item{cex}{The character expansion to use in the display.}
}
\details{  
 \samp{compassRose} displays a conventional compass rose at the position
 requested. The size of the compass rose is determined by the character
 expansion, as the central "rose" is calculated relative to the character size.
 Rotation is in degrees counterclockwise.
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
