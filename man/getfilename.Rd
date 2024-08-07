% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{getfilename}
\alias{getfilename}
\alias{get_dbf_file_name}
\alias{get_dat_file_name}
\alias{get_dat_common_name}
\alias{get_common_dat_names}
\title{Get File Name(s)}
\usage{
get_dbf_file_name(dbf_file)

get_dat_file_name(dat_file)

get_dat_common_name(dat_file)

get_common_dat_names(dat_files)
}
\arguments{
\item{dbf_file}{DBF file}

\item{dat_file}{DAT file}

\item{dat_files}{DAT files}
}
\value{
file

name

name

name

name
}
\description{
Get File Name(s)
}
\examples{
\dontrun{
get_dbf_file_name(dbf_file = "liposomes_218.dbf")
get_dat_file_name(dat_file = "dat_1.dat")
get_common_dat_names(dat_files = list.files(fpath, pattern = "\\\\.dat$"))}
}
\concept{getfilename}
