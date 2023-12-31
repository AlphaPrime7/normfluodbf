% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample_dat_vect.R
\name{resample_vect_scale}
\alias{resample_vect_scale}
\title{Title: A function to create an attribute or column for each sample loaded into the microplate wells.}
\usage{
resample_vect_scale(df, tnp, cycles, method = c("normal", "brute", "vector"))
}
\arguments{
\item{df}{A clean data frame with attributes or tuples containing a mixture of samples.}

\item{tnp}{A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.}

\item{cycles}{A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.}

\item{method}{A string 'normal', 'brute' or 'vector' to specify the method of resampling.}
}
\value{
A new data frame where separated samples are assigned a separate attribute or column.
}
\description{
Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
}
\note{
This is the pseudo-vectorized approach and should be a more efficient function.
This function will produce a vertical layout as defined in this package.
This function inspired by the lapply approach pretty much applies the
}
\examples{
fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
dat_df <- read.table(file=fpath)
nocomma_dat <- clean_odddat_optimus(dat_df)
alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'brute')
alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'normal')
alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'vector')
alt_test_scale_norm <- lapply(alt_test_scale, min_max_norm)
}
\seealso{
\code{\link[=resample_dat_vect]{resample_dat_vect()}}

\code{\link[=resample_dat_vect]{resample_dat_vect()}}. As a matter of fact, I took this approach to
create compatibility with lapply and rapply but that failed.
}
\author{
Tingwei Adeck
}
