% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plate_steps.R
\name{stepspipeline}
\alias{stepspipeline}
\alias{.next_step}
\alias{next_step}
\alias{run_steps}
\title{Steps Pipeline}
\usage{
.next_step(plate, n = 1)

next_step(plate, n = 1)

run_steps(plate, reset = FALSE, ...)
}
\arguments{
\item{plate}{plate}

\item{n}{n}

\item{reset}{reset}

\item{...}{dots}
}
\value{
plate

plate

plate

plate
}
\description{
Steps Pipeline
}
\note{
Recursive function to implement steps in the plate until all steps in the pipeline are complete

Recursive function to implement steps in the plate until all steps in the pipeline are complete
}
\examples{
\dontrun{next_step(plate, n=1)}
}
\concept{stepspipeline}
