% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plate_steps.R
\name{definesteps}
\alias{definesteps}
\alias{define_steps}
\alias{define_steps.default}
\alias{define_steps.normfluodbf_plate}
\alias{define_steps.96well_plate}
\alias{define_steps.384well_plate}
\alias{define_steps.1536well_plate_t1}
\alias{define_steps.1536well_plate_t2}
\alias{set_default_steps}
\alias{update_steps_list}
\title{Define Plate Steps}
\usage{
define_steps(plate)

\method{define_steps}{default}(plate)

\method{define_steps}{normfluodbf_plate}(plate)

\method{define_steps}{`96well_plate`}(plate)

\method{define_steps}{`384well_plate`}(plate)

\method{define_steps}{`1536well_plate_t1`}(plate)

\method{define_steps}{`1536well_plate_t2`}(plate)

set_default_steps(plate, ...)

update_steps_list(plate, new_key, new_value, index)
}
\arguments{
\item{plate}{plate}

\item{...}{custom steps}

\item{new_key}{new_key}

\item{new_value}{new_value}

\item{index}{index}
}
\value{
steps

steps

steps

steps

steps

steps

plate

plate
}
\description{
Define Plate Steps
}
\examples{
\dontrun{define_steps(plate)}
\dontrun{plate <- plate \%>\% update_steps_list('REMOVE_OUTLIER', 'remove_outlier', 3)}
}
\concept{definesteps}
