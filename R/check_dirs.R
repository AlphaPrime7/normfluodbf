#' Title: A function to check for DBFs in a directory.
#'
#' @description
#' A function that facilitates a users' workflow by helping to check for DBFs in a directory.
#'
#' @author Tingwei Adeck
#'
#' @param pathstring A string for a path to a directory containing files.
#'
#' @return Returns a list of DBF files or a warning.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' check_dbf(getwd())
#' }

check_dbf <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$")) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$")
    return(files_list)
  } else{
    warning("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}

#' Title: A function to check for DATs in a directory.
#'
#' @description
#' A function that facilitates a users' workflow by helping to check for DATs in a directory.
#'
#' @author Tingwei Adeck
#'
#' @param pathstring A string for a path to a directory containing files.
#'
#' @return Returns a list of DAT files or a warning.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' check_dat(getwd())
#' }

check_dat <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dat$")) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dat$")
    return(files_list)
  } else{
    warning("No .dat files in pwd. Change the directory to one with .dat files")
  }
}
