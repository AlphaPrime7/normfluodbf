## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Set The Child Plate Type
#' @param parent_plate plate
#' @param type child plate type
#' @return plate
#' @details
#' This is a recursive method.
#' @export
#' @examples
#' \dontrun{
#' x = set_plate_type(parent_plate,"96well_plate")
#' print(class(x))
#' }
set_plate_type <- function(parent_plate, type) {
  if (length(class(parent_plate)) == 3 && class(parent_plate)[1] != "list") {
    return(parent_plate)
  }

  if (missing(type) || is.null(type) ) {
    type <- "96well_plate"
  }

  child_class <- type
  if (class(parent_plate)[1] != "list") {
    child_class <- c(child_class, class(parent_plate))
  }
  class(parent_plate) <- child_class
  parent_plate

  set_plate_type(parent_plate, child_plate_type(type))
}

#' Set The Plate Types
#' @param parent_plate A parent plate
#' @param type parent plate type
#' @param child_type child plate type
#' @return plate
#' @details
#' Non-recursive approach.
#' @export
#' @examples \dontrun{set_assiette_type(parent_plate_type,96well_plate)}
set_assiette_type <- function(plate, type=NULL, child_type=NULL){
  if (length(class(plate)) == 3 && class(plate)[1] != "list"){
    return(plate)
  }

  if (is.null(type)){
    type = "normfluodbf_plate"
  }

  if (class(plate)[1] != "list"){
    class(plate) <- "list"
  }

  plate = plate %>% parent_plate_type(type = type) %>% child_plate_type()
  plate
}
