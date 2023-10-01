\name{GridsDatums}
\alias{GridsDatums}
\docType{data}
\title{Grids and Datums PE&RS listing}
\description{A data.frame of years and months of Grids & Datums column publications by country and country code.}
\usage{data("GridsDatums")}
\format{
  A data frame with 241 observations on the following 4 variables.
  \describe{
    \item{\code{country}}{name of PE&RS column}
    \item{\code{month}}{issue month}
    \item{\code{year}}{publication year}
    \item{\code{ISO}}{ISO code for country}
  }
}
\details{The journal \emph{Photogrammetric Engineering & Remote Sensing}, run by the American Society for Photogrammetry and Remote Sensing (ASPRS), began publishing a more-or-less monthly column on the spatial reference systems used in different countries, including their datums. The column first appeared in September 1997, and continued until March 2016; subsequent columns are updated reprints of previous ones. Some also cover other topics, such as world and Martian spatial reference systems. They are written by Clifford J. Mugnier, Louisiana State University, Fellow Emeritus ASPRS. To access the columns, visit \url{https://www.asprs.org/asprs-publications/grids-and-datums}.}
\source{\url{https://www.asprs.org/asprs-publications/grids-and-datums}}
\examples{
data(GridsDatums)
GridsDatums[grep("Norway", GridsDatums$country),]
GridsDatums[grep("Google", GridsDatums$country),]
GridsDatums[grep("^Mars$", GridsDatums$country),]
}
\keyword{datasets}
