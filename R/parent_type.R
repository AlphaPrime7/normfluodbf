## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Parent Type
#' @family parenttype
#' @param plate plate
#' @param type parent type
#' @return NULL
#' @name parenttype
#' @examples
#' \dontrun{
#' parent_plate_type()
#' }
NULL

#' @rdname parenttype
#' @return NULL
#' @export
parent_plate_type <- function(plate,type=NULL) {
  UseMethod("parent_plate_type")
}

#' @rdname parenttype
#' @return NULL
#' @export
parent_plate_type.normfluodbf_plate <- function(plate,type=NULL) {
  NULL
}

#' @rdname parenttype
#' @return plate
#' @export
parent_plate_type.default <- function(plate,type=NULL) {

  if (is.null(type)){
    type = "normfluodbf_plate"
  }
  else {
    type = type
  }

  if(class(plate)[1] == type || class(plate)[1] == "normfluodbf_plate"){
    plate
  } else {
    class(plate) = "list"
    class(plate) = c(type, class(plate))
    plate
  }
}

#' @rdname parenttype
#' @return plate
#' @export
parent_plate_type.96well_plate <- function(plate,type=NULL) {

  if (is.null(type)){
    type = "normfluodbf_plate"
  }
  else {
    type = type
  }

  if(class(plate)[1] == type || class(plate)[1] == "normfluodbf_plate"){
    plate
  } else {
    class(plate) = "list"
    class(plate) = c(type, class(plate))
    plate
  }
}

#' @rdname parenttype
#' @return plate
#' @export
parent_plate_type.384well_plate <- function(plate,type=NULL) {

  if (is.null(type)){
    type = "normfluodbf_plate"
  }
  else {
    type = type
  }

  if(class(plate)[1] == type || class(plate)[1] == "normfluodbf_plate"){
    plate
  } else {
    class(plate) = "list"
    class(plate) = c(type, class(plate))
    plate
  }
}

#' @rdname parenttype
#' @return plate
#' @export
parent_plate_type.1536well_plate_t1 <- function(plate,type=NULL) {

  if (is.null(type)){
    type = "normfluodbf_plate"
  }
  else {
    type = type
  }

  if(class(plate)[1] == type || class(plate)[1] == "normfluodbf_plate"){
    plate
  } else {
    class(plate) = "list"
    class(plate) = c(type, class(plate))
    plate
  }
}

#' @rdname parenttype
#' @return plate
#' @export
parent_plate_type.1536well_plate_t2 <- function(plate,type=NULL) {

  if (is.null(type)){
    type = "normfluodbf_plate"
  }
  else {
    type = type
  }

  if(class(plate)[1] == type || class(plate)[1] == "normfluodbf_plate"){
    plate
  } else {
    class(plate) = "list"
    class(plate) = c(type, class(plate))
    plate
  }
}
