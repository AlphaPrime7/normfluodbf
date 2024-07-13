## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Child Type
#' @family childtype
#' @param plate plate
#' @param child_type child type
#' @return NULL
#' @name childtype
#' @examples
#' \dontrun{child_plate_type()}
#'
NULL

#' @rdname childtype
#' @return NULL
#' @export
child_plate_type <- function(plate,child_type=NULL) {
  UseMethod("child_plate_type")
}

#' @rdname childtype
#' @return plate
#' @export
child_plate_type.default <- function(plate,child_type=NULL) {

  plate_types = plate_types()
  formatted_string <- sprintf("child_type option is %s.", plate_types())
  print(formatted_string)

  if (is.null(child_type)){
    child_type = "96well_plate"
  } else {
    child_type = child_type
  }

  if(class(plate)[1] != "normfluodbf_plate"){
    plate = parent_plate_type(plate)
    class(plate) = c(child_type, class(plate))
    plate
  } else {
    class(plate) = c(child_type, class(plate))
    plate
  }
}

#' @rdname childtype
#' @return plate
#' @export
child_plate_type.normfluodbf_plate <- function(plate,child_type=NULL) {

  plate_types = plate_types()
  formatted_string <- sprintf("child_type option is %s.", plate_types())
  print(formatted_string)

  if (is.null(child_type)){
    child_type = "96well_plate"
  } else {
    child_type = child_type
  }

  if(class(plate)[1] != "normfluodbf_plate"){
    plate = parent_plate_type(plate)
    class(plate) = c(child_type, class(plate))
    plate
  } else {
    class(plate) = c(child_type, class(plate))
    plate
  }
}

#' Plate Types Tibble
#' @return A tibble
#' @export
#' @seealso [plate_types()]
#' @name plate_types_tbl
plate_types_tbl <- function(){

  plate_type_tbl <- tibble::tribble(
    ~plate_types, ~Value,
    "96well_plate", "96well_plate",
    "384well_plate", "384well_plate",
    "1536well_plate_t1", "1536well_plate_t1",
    "1536well_plate_t2", "1536well_plate_t2")

  class(plate_type_tbl) <- c("normfluodbf_plate_types", class(plate_type_tbl))
  return(plate_type_tbl)
}

#' Plate Types List
#' @return A list
#' @export
#' @details
#' The list equivalent of the tibble from \code{plate_types_tbl}.
#' @name plate_types_list
plate_types <- function(){

  plate_types <- list()
  plate_types[["96well_plate"]]  <- "96well_plate"
  plate_types[['384well_plate']] <- "384well_plate"
  plate_types[['1536well_plate_t1']] <- "1536well_plate_t1"
  plate_types[['1536well_plate_t2']] <- "1536well_plate_t2"

  class(plate_types) <- c("normfluodbf_plate_types", class(plate_types))
  return(plate_types)
}

#' Plate Types Vector
#' @return A Vector
#' @export
#' @details
#' The vector equivalent of the tibble from \code{plate_types_tbl}.
#' @name plate_types_vector
plate_types_vector <- function(){

  pt = c('96well_plate',
         '384well_plate',
         '1536well_plate_t1',
         '1536well_plate_t2')

  class(pt) <- c("normfluodbf_plate_types", class(pt))
  return(pt)
}
