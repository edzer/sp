\name{meuse}
\alias{meuse}
\docType{data}
\title{Meuse river data set}
\description{
This data set gives locations and topsoil heavy metal concentrations, along with a number of soil and landscape variables at the observation locations, collected in a flood plain of the river Meuse, near the village of Stein (NL). Heavy metal concentrations are from composite samples of an area of approximately 15 m x 15 m.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{a numeric vector; Easting (m) in Rijksdriehoek (RDH)
    (Netherlands topographical) map coordinates }
    \item{y}{a numeric vector; Northing (m) in RDH  coordinates }
	\item{cadmium}{topsoil cadmium concentration, mg kg-1 soil ("ppm"); zero cadmium
	values in the original data set have been shifted to 0.2 (half the lowest 
	non-zero value) }
	\item{copper}{topsoil copper concentration, mg kg-1 soil  ("ppm") }
	\item{lead}{topsoil lead concentration, mg kg-1 soil  ("ppm") }
	\item{zinc}{topsoil zinc concentration, mg kg-1 soil  ("ppm") } 
	\item{elev}{relative elevation above local river bed, m }
	\item{dist}{distance to the Meuse; obtained from the nearest cell in
	\link{meuse.grid}, which in turn was derived by a spread (spatial distance)
	GIS operation, horizontal precision 20 metres; then
	normalized to $[0,1]$ }
	\item{om}{organic matter, kg (100 kg)-1 soil (percent)}
	\item{ffreq}{flooding frequency class: 1 = once in two years; 2 = once in ten years; 3 = one in 50 years}
	\item{soil}{soil type according to the 1:50 000 soil map of the Netherlands. 1 = Rd10A (Calcareous weakly-developed meadow soils, light sandy clay); 2 = Rd90C/VII (Non-calcareous weakly-developed meadow soils, heavy sandy clay to light clay); 3 = Bkd26/VII (Red Brick soil, fine-sandy, silty light clay)}
	\item{lime}{lime class: 0 = absent, 1 = present by field test with 5\% HCl}
	\item{landuse}{landuse class: Aa  Agriculture/unspecified = , Ab = Agr/sugar beetsm, Ag = Agr/small grains, Ah = Agr/??, Am = Agr/maize, B = woods, Bw  = trees in pasture, DEN = ??, Fh = tall fruit trees, Fl = low fruit trees; Fw = fruit trees in pasture, Ga = home gardens, SPO = sport field, STA = stable yard, Tv = ??
, W = pasture}
	\item{dist.m}{distance to river Meuse in metres, as obtained during the field survey}
  }
}
\usage{
data(meuse)
}
\note{row.names refer to the original sample number.

Soil units were mapped with a minimum delination width of 150 m, and so somewhat generalize the landscape.

Approximate equivalent World Reference Base 2002 for Soil Resources names are: Rd10A Gleyic Fluvisols; Rd90C Haplic Fluvisols; Bkd26 Haplic Luvisols. Units Rd90C and Bkd26 have winter groundwater > 80cm, summer > 120cm depth.
}

\author{Field data were collected by Ruud van Rijn and
Mathieu Rikken; compiled for R by Edzer Pebesma; description 
extended by David Rossiter }

\references{
M G J Rikken and R P G Van Rijn, 1993. Soil pollution with heavy metals - an inquiry into spatial variation, cost of mapping and the risk evaluation of copper, cadmium, lead and zinc in the floodplains of the Meuse west of Stein, the Netherlands. Doctoraalveldwerkverslag, Dept. of Physical Geography, Utrecht University

P.A. Burrough, R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press.

Stichting voor Bodemkartering (STIBOKA), 1970. Bodemkaart van Nederland : Blad 59 Peer, Blad 60 West en 60 Oost Sittard: schaal 1 : 50 000. Wageningen, STIBOKA.

\url{http://www.gstat.org/}
}
\keyword{datasets}

\examples{
data(meuse)
summary(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
}
