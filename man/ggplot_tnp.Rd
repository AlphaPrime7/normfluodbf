% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_tnp.R
\name{ggplot_tnp}
\alias{ggplot_tnp}
\title{Title: A Visualization function.}
\usage{
ggplot_tnp(df, x, y_list, xlim, ylim)
}
\arguments{
\item{df}{A clean data frame with attributes or tuples containing a mixture of samples.}

\item{x}{The X-variable often the cycle number OR time.}

\item{y_list}{A character vector of samples that need to be plotted. Often of the format TNP (Test, Negative, Positive).}

\item{xlim}{The X-variable scale.}

\item{ylim}{The Y-variable scale.}
}
\value{
A nice visual of the clean and normalized data frame.
}
\description{
A visualization function using ggplot2.
}
\examples{
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
dat_df <- normfluodat(fpath,3,40, rows_used = c('A','B','C'), norm_scale = 'one')
yvars <- c("A1","B1","C1")
xvar <- c("Cycle_Number", "Time")
xl <- c(0,40)
yl <- c(0,1)
ggplot_tnp(dat_df,xvar,yvars,xl,yl)
}
\author{
Tingwei Adeck
}