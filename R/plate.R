## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plate
#' @family plate
#' @param plate plate
#' @param type parent type
#' @param child_type child type
#' @return plate
#' @name plate
#' @examples
#' \dontrun{
#' empty_plate()
#' new_plate()
#' init_plate()
#' setup_plate(plate)
#' reset_plate()
#' }
NULL

#' @rdname plate
#' @return plate
#' @export
empty_plate <- function() {
  list(
    plate_meta = NULL,
    plate_data = NULL,
    steps = NULL,
    status = NULL,
    dirty = NULL,
    version = NULL)
}

#' @rdname plate
#' @return plate
#' @export
new_plate = function(){
  empty_plate()
}

#' @rdname plate
#' @return plate
#' @export
init_plate = function(plate=NULL, type=NULL, child_type=NULL){

  if (is.null(plate) || missing(plate)){
    plate = new_plate()
  } else {
    plate = plate
  }

  if (class(plate)[1] != 'normfluodbf_plate'){
    plate = parent_plate_type(plate,type=type)
  }
  plate = set_plate_type(plate, child_type)
  plate = set_plate_version(plate,'normfluodbf')
  plate = check_dirt(plate)
  plate
  .globals$set("init_plate", plate)
}

#' @rdname plate
#' @return plate
#' @export
setup_plate = function(plate, ...){
  stopifnot(plate %>% inherits("normfluodbf_plate"))

  if (!missing(...)){
    params(plate) %<>% utils::modifyList(params)
  }

  if (!missing(...)){
    name(plate) %<>% name
  }

  plate = plate %>%
    set_default_params %>%
    set_default_steps %>%
    set_default_status

  load_plate_meta(plate) = plate_meta(plate)

  CURRENT_STATUS <- plate %>% step('NO_DATA')

  if (is.null(plate[['plate_data']]) && !is_plate_dirty(plate)){
    status(plate) = plate[['status']][[CURRENT_STATUS]]
    steps(plate) =  plate[['steps']][-1]
  }
  plate
  .globals$set("setup_plate", plate)
}

#' @rdname plate
#' @return plate
#' @export
reset_plate = function(plate){

  if (is_plate_dirty(plate)){
    usr_choice = utils::menu(c("yes",
                  "no",
                  "hello world"),
                  title = "Hey Yoda, are you sure you want to reset the plate? (Save the plate before resetting!)")
    if (usr_choice == "no") {
      plate
    }
    else {
      plate = NULL
      plate = init_plate(plate)
      plate = setup_plate(plate)
      plate
    }
  }
}

#' @rdname plate
#' @return plate
#' @export
use_setup_plate = function(){
  #stopifnot(.globals$get("setup_plate") %>% inherits("normfluodbf_plate"))
  plate <- .globals$get("setup_plate")
  plate
}

#' @rdname plate
#' @return plate
#' @export
use_initialized_plate = function(){
  plate <- .globals$get("init_plate")
  plate
}


