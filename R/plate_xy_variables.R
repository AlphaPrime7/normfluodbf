## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plot Coordinates
#' @family xycoordinates
#' @param plate plate
#' @param value value
#' @return NULL
#' @name xycoordinates
#' @examples
#' \dontrun{x_var_one(plate,value)}
NULL

#' @rdname xycoordinates
#' @return plate
#' @export
x_var_one <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_ONE')
}

#' @rdname xycoordinates
#' @return plate
#' @export
`x_var_one<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_ONE') <- value
  plate
}

#' @rdname xycoordinates
#' @return plate
#' @export
x_var_two <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_TWO')
}

#' @rdname xycoordinates
#' @return plate
#' @export
`x_var_two<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_TWO') <- value
  plate
}

#' @rdname xycoordinates
#' @return plate
#' @export
y_var <- function(plate) {
  params(plate, 'GENERAL', 'Y_VAR')
}

#' @rdname xycoordinates
#' @return plate
#' @export
`y_var<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'Y_VAR') <- value
  plate
}

#' @rdname xycoordinates
#' @return plate
#' @export
x_var_one_label <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_ONE_LABEL')
}

#' @rdname xycoordinates
#' @return plate
#' @export
`x_var_one_label<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_ONE_LABEL') <- value
  plate
}

#' @rdname xycoordinates
#' @return plate
#' @export
x_var_two_label <- function(plate) {
  params(plate, 'GENERAL', 'X_VAR_TWO_LABEL')
}

#' @rdname xycoordinates
#' @return plate
#' @export
`x_var_two_label<-` <- function(plate, value) {
  params(plate, 'GENERAL', 'X_VAR_TWO_LABEL') <- value
  plate
}




