% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize}
\alias{normalize}
\alias{normalize.default}
\alias{normalize.96well_plate}
\alias{normalize.384well_plate}
\alias{normalize.1536well_plate_t1}
\alias{normalize.1536well_plate_t2}
\alias{normalize_dataframe}
\title{Normalize}
\usage{
normalize(plate)

\method{normalize}{default}(plate)

\method{normalize}{`96well_plate`}(plate)

\method{normalize}{`384well_plate`}(plate)

\method{normalize}{`1536well_plate_t1`}(plate)

\method{normalize}{`1536well_plate_t2`}(plate)

normalize_dataframe(df)
}
\arguments{
\item{plate}{plate}

\item{df}{data frame}
}
\value{
plate

plate

plate

plate

plate

plate

plate

plate
}
\description{
Normalize
}
\examples{
\dontrun{normalize(plate)
normalize(plate)}
}
\concept{normalize}
