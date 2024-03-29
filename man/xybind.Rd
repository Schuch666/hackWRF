% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xybind.R
\name{ccbind}
\alias{ccbind}
\alias{crbind}
\title{Combine R Objects by Columns or Rows}
\usage{
ccbind(x, y, bind = TRUE, message = NA)

crbind(x, y, bind = TRUE, message = NA)
}
\arguments{
\item{x}{first element to combine}

\item{y}{second element to combine}

\item{bind}{condition (TRUE or FALSE) for combine x and y or return only x}

\item{message}{message in case of bind is false (no binding)}
}
\description{
This function perform a conditional cbind or rbind.
}
\section{Functions}{
\itemize{
\item \code{ccbind()}: Perform a conditional cbind

\item \code{crbind()}: Perform a conditional rbind

}}
\examples{
a <- 1:10
b <- 2:11

ccbind(a,b,length(a) == length(b))
ccbind(a,b,length(a) == 12, message = 'returning x = a')
crbind(a,b,length(a) > 9)
}
