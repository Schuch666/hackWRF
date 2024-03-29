% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis.R
\name{longitude}
\alias{longitude}
\alias{latitude}
\alias{longitude_proj}
\alias{latitude_proj}
\alias{grid_proj}
\title{PLot latitude and longitude}
\usage{
longitude(int = 10, side = 1, lmin = -180, lmax = 180, ...)

latitude(int = 10, side = 2, lmin = -90, lmax = 90, ...)

longitude_proj(r, int = 10, side = 1, lmin = -180, lmax = 180, ...)

latitude_proj(r, int = 10, side = 2, lmin = -80, lmax = 80, ...)

grid_proj(
  r,
  int = 10,
  lty = 3,
  col = "#666666",
  lat_min = -80,
  lat_max = 80,
  lon_min = -160,
  lon_max = 160,
  ...
)
}
\arguments{
\item{int}{interval in degrees}

\item{side}{side to plot, see axis}

\item{lmin}{minimum lat or lon}

\item{lmax}{maximum lat or lon}

\item{...}{additional arguments passed to axis function}

\item{r}{a raster object (to projected versions)}

\item{lty}{line type for grid_projected (default is 3)}

\item{col}{line color for grid_projected (default is gray)}

\item{lat_min}{for rid_projected (default is -80)}

\item{lat_max}{for rid_projected (default is 30)}

\item{lon_min}{for rid_projected (default is -160)}

\item{lon_max}{for rid_projected (default is 160)}
}
\description{
function to plot cool latitude and longitude.
labels for latitude range from 180ºW to 0 to 180ºE and longitude from 90ºS to 0 to 90ºN.
}
\section{Functions}{
\itemize{
\item \code{longitude()}: nice latitude axis

\item \code{latitude()}: nice longitude axis

\item \code{longitude_proj()}: projected longitude axis

\item \code{latitude_proj()}: projected latitude axis

\item \code{grid_proj()}: grid (lalitude and longitude) in a different projection

}}
\examples{
library(raster)
br <- shapefile(paste0(system.file("extdata",package="hackWRF"),"/BR_states.shp"),verbose=FALSE)
plot(br)
box()
latitude()
longitude()
grid()

}
