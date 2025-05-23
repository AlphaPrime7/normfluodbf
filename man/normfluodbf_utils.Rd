% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normfluodbf_utils.R
\name{unique_identifier}
\alias{unique_identifier}
\alias{generic_identifier}
\alias{norm_applier}
\alias{time_attribute}
\alias{actual_rows_used}
\alias{get_tnp}
\alias{actual_cycles}
\alias{actual_cols_used}
\title{A function to append a unique identifier attribute to any data frame within the normfluodbf package.}
\usage{
unique_identifier(df)

generic_identifier(numrows, col_name)

norm_applier(df, norm_scale = c("one", "hundred", "z-score", "raw", "decimal"))

time_attribute(
  interval = NULL,
  first_end = NULL,
  pause_duration = NULL,
  end_time = NULL,
  cycles = NULL
)

actual_rows_used(dat)

get_tnp(dat)

actual_cycles(dat)

actual_cols_used(dat)
}
\arguments{
\item{df}{A data frame.}

\item{numrows}{The number of rows the user intends to have in the created data frame.}

\item{col_name}{The desired attribute name.}

\item{norm_scale}{This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
which denotes the normalization type or scale.}

\item{interval}{The time interval chosen for the assay often in seconds.}

\item{first_end}{The end time of the initial run, often the pause for the introduction of a new substance. This can be the cycle number chosen for the initial stop.}

\item{pause_duration}{The time between the first end (pause) and resumption of the assay.}

\item{end_time}{The final end time of the assay.}

\item{cycles}{The number of cycles in the assay as selected by the user or researcher.}

\item{dat}{A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.}
}
\value{
A data frame with the Cycle_Number attribute appended to the end of the data frame.

A user-named single attribute data frame with nrow = numrows.

A data frame with attribute values obtained from the applied function using lapply.

The time attribute.

Returns a character vector denoting the rows used in the assay.

Returns the number of rows used denoted as tnp.

The number of cycles.

Returns a numeric vector denoting the columns used in the assay.
}
\description{
The function in the context of normfluodbf creates an attribute called Cycle_Number
and appends this attribute to the cleaned or wrangled data frame derived from the dirty DBF file.

A function that creates an attribute of seq(numrows) with a step size of 1, where the user provides the attribute name.

Applies a function over a list of attributes.

A function to help the researcher reproduce the time attribute.

A function that facilitates a users' workflow by helping extract the actual rows used in the assay.

A function that facilitates a users' workflow by helping to get the number of rows used in the assay.

A function to get the number of cycles used in the assay.

A function that facilitates a users' workflow by helping extract the actual columns used in the assay.
}
\note{
The function operates in a closed system,
meaning it is primarily designed to work with this package ONLY.
Other use cases are simply a coincidence.

The original function had an option for minutes which was for less time conscious people
but the final version for this package has no such option. Users MUST provide numbers in
seconds.
}
\examples{
\dontrun{
test_df <- as.data.frame(c(seq(40)))
colnames(test_df) <- "test"
unique_identifier(test_df)}
\dontrun{
generic_identifier(40, col_name="Cycle_No")}
\dontrun{
test_df <- as.data.frame(c(seq(40)))
colnames(test_df) <- "test"
test_df_norm <- norm_applier(test_df,norm_scale = 'one')}
\dontrun{
time_test = time_attribute(30,8,136,1276,40)
time_test = time_attribute(60,8,136,2460,40)}
\dontrun{
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
arutest <- actual_rows_used(fpath)}
\dontrun{
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
gettnptest <- get_tnp(fpath)}
\dontrun{
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
getcyclestest <- actual_cycles(fpath)}
\dontrun{
fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
acutest <- actual_cols_used(fpath)}
}
\seealso{
\code{\link[=normfluodat]{normfluodat()}}
}
\author{
Tingwei Adeck
}
\keyword{internal}
