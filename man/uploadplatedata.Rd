% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plate_data.R
\name{uploadplatedata}
\alias{uploadplatedata}
\alias{upload_data}
\alias{upload_data.default}
\alias{upload_data.96well_plate}
\alias{upload_data.384well_plate}
\alias{upload_data.1536well_plate_t1}
\alias{upload_data.1536well_plate_t2}
\title{Upload Plate Data}
\usage{
upload_data(plate, file, ...)

\method{upload_data}{default}(plate, file, ...)

\method{upload_data}{`96well_plate`}(plate, file, ...)

\method{upload_data}{`384well_plate`}(plate, file, ...)

\method{upload_data}{`1536well_plate_t1`}(plate, file, ...)

\method{upload_data}{`1536well_plate_t2`}(plate, file, ...)
}
\arguments{
\item{plate}{plate}

\item{file}{file}

\item{...}{dots}
}
\value{
plate

plate

plate

plate

plate

plate

plate
}
\description{
Upload Plate Data
}
\examples{
\dontrun{upload_data(plate, file, ...)}
}
\concept{uploadplatedata}
