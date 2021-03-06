% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_emissions.R
\name{merge_emission}
\alias{merge_emission}
\title{Function to merge two different emissions}
\usage{
merge_emission(
  background,
  auxiliar,
  output = NA,
  name,
  proj = NA,
  plot = T,
  verbose = T
)
}
\arguments{
\item{background}{main input file (WRF-Chem emission file)}

\item{auxiliar}{auxliar input file (WRF-Chem emission file)}

\item{output}{output file (WRF-Chem emission file), or NA to return the emission array}

\item{name}{variable/pollutant name}

\item{proj}{projection to convert the raster, default is NA (no conversion)}

\item{plot}{to plot from output file}

\item{verbose}{display additional information}
}
\value{
if output is NA, returns an array or a matrix
}
\description{
Merge different emissions, the main emission or background emission
(usually from a global inventory) and an auxiliary emission. The resulting emissions
has the values of the auxiliar emissions for grid points with emissions and background
emission for grid points with zero emissions.
}
\examples{
\dontrun{
NEW_e <- merge_emission(background = 'base/wrfchemi_00z_d01',
                        auxiliar   = 'extra/wrfchemi_00z_d01',
                        name       = 'E_NO')
}
}
