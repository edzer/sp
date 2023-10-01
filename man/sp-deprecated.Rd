\name{sp-deprecated}
%\alias{overlay}
%\alias{overlay-methods}
%\alias{overlay,SpatialPointsDataFrame,SpatialPolygons-method}
%\alias{overlay,SpatialPoints,SpatialPolygons-method}
%\alias{overlay,SpatialPoints,SpatialPolygonsDataFrame-method}
%\alias{overlay,SpatialPolygons,SpatialPoints-method}
%\alias{overlay,SpatialGridDataFrame,SpatialPolygons-method}
%\alias{overlay,SpatialGrid,SpatialPolygons-method}
%\alias{overlay,SpatialGrid,SpatialPolygonsDataFrame-method}
%\alias{overlay,SpatialPolygons,SpatialGrid-method}
%\alias{overlay,SpatialGridDataFrame,SpatialPoints-method}
%\alias{overlay,SpatialGrid,SpatialPoints-method}
%\alias{overlay,SpatialPixelsDataFrame,SpatialPoints-method}
%\alias{overlay,SpatialPixels,SpatialPoints-method}
\alias{getSpPPolygonsIDSlots}
\alias{getSpPPolygonsLabptSlots}
\alias{getSpPnParts}
\alias{getSpPnHoles}
\alias{getSpPplotOrderSlot}
\alias{getSpPpolygonsSlot}
\alias{getPolygonAreaSlot}
\alias{getPolygonCoordsSlot}
\alias{getPolygonLabptSlot}
\alias{getPolygonHoleSlot}
\alias{getPolygonsIDSlot}
\alias{getPolygonsLabptSlot}
\alias{getPolygonsPolygonsSlot}
\alias{getPolygonsplotOrderSlot}
\alias{getLinesLinesSlot}
\alias{getLinesIDSlot}
\alias{getSLlinesSlot}
\alias{getSLLinesIDSlots}
\alias{get_evolution_status}
\alias{set_evolution_status}

\title{ Deprecated functions in sp }
\description{
Deprecated functions is sp: getSpP*, getPolygon*, getLines* getSL*
}
\usage{
set_evolution_status(value)
get_evolution_status()
}
\arguments{
\item{value}{evolution status: only 2L to use sf functions in place of rgdal}
}

\note{ For \code{overlay} the new implementation is found in the \link{over} 
method; this works slightly different and more consistent. }
\keyword{manip}
