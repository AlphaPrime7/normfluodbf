## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Print
#' @family printer
#' @param plate plate
#' @param ... placeholder
#' @return NULL
#' @name printer
#' @examples
#' \dontrun{plate}
#'
NULL

#' @rdname printer
#' @return plate
#' @export
print.96well_plate <- function(plate, ...){
  if ( length(steps(plate)) == 1 ){
    l = 0
  }
  else {
    l = length(steps(plate))
  }

  cat(sprintf("#>               Parent Plate Type: %s\n", class(plate)[2]) )
  cat(sprintf("#>               Child Plate type : %s\n", class(plate)[1]))
  cat("#>                       -------------\n")
  cat(sprintf("#>               Dataset name : %s\n", plate$dataset_name))
  cat(sprintf("#>               Data summary : %s\n", plate$data_summary))
  cat(sprintf("#>               Completed processes : %s\n", status(plate)))
  cat(sprintf("#>               Remaining processes : %s\n", l ))

}

#' @rdname printer
#' @return plate
#' @export
print.384well_plate <- function(plate, ...){
  if ( length(steps(plate)) == 1 ){
    l = 0
  }
  else {
    l = length(steps(plate))
  }

  cat(sprintf("#>               Parent Plate Type: %s\n", class(plate)[2]) )
  cat(sprintf("#>               Child Plate type : %s\n", class(plate)[1]))
  cat("#>                       -------------\n")
  cat(sprintf("#>               Dataset name : %s\n", plate$dataset_name))
  cat(sprintf("#>               Data summary : %s\n", plate$data_summary))
  cat(sprintf("#>               Completed processes : %s\n", status(plate)))
  cat(sprintf("#>               Remaining processes : %s\n", l ))

}

#' @rdname printer
#' @return plate
#' @export
print.1536well_plate_t1 <- function(plate, ...){
  if ( length(steps(plate)) == 1 ){
    l = 0
  }
  else {
    l = length(steps(plate))
  }

  cat(sprintf("#>               Parent Plate Type: %s\n", class(plate)[2]) )
  cat(sprintf("#>               Child Plate type : %s\n", class(plate)[1]))
  cat("#>                       -------------\n")
  cat(sprintf("#>               Dataset name : %s\n", plate$dataset_name))
  cat(sprintf("#>               Data summary : %s\n", plate$data_summary))
  cat(sprintf("#>               Completed processes : %s\n", status(plate)))
  cat(sprintf("#>               Remaining processes : %s\n", l ))

}

#' @rdname printer
#' @return plate
#' @export
print.1536well_plate_t2 <- function(plate, ...){
  if ( length(steps(plate)) == 1 ){
    l = 0
  }
  else {
    l = length(steps(plate))
  }

  cat(sprintf("#>               Parent Plate Type: %s\n", class(plate)[2]) )
  cat(sprintf("#>               Child Plate type : %s\n", class(plate)[1]))
  cat("#>                       -------------\n")
  cat(sprintf("#>               Dataset name : %s\n", plate$dataset_name))
  cat(sprintf("#>               Data summary : %s\n", plate$data_summary))
  cat(sprintf("#>               Completed processes : %s\n", status(plate)))
  cat(sprintf("#>               Remaining processes : %s\n", l ))

}
