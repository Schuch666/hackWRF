% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster_to_ncdf.R
\name{raster_to_ncdf}
\alias{raster_to_ncdf}
\title{Convert a raster or brick to a array for WRF-Chem}
\usage{
raster_to_ncdf(r, na_value = 0)
}
\arguments{
\item{r}{input raster}

\item{na_value}{value for missing values}
}
\value{
array
}
\description{
Conversion a raster or brick to a array for WRF-Chem model
}
\examples{

r <- raster::raster(paste0(system.file("extdata", package = "hackWRF"),
                           "/wrf.day1.o3.nc"))
a <- raster_to_ncdf(r)

}
