## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Define Plate Status
#' @family definestatus
#' @param plate plate
#' @param index index
#' @return plate
#' @name definestatus
#' @examples
#' \dontrun{define_steps(plate)}
NULL

#' @rdname definestatus
#' @return status
#' @export
define_status = function(plate){
  UseMethod('define_status')
}

#' @rdname definestatus
#' @return status
#' @export
define_status.default <- function(plate) {
  list(
    'NO_DATA' = 0,
    'DATA_INITIALIZED' = 1,
    'FORMAT_DATA' = 2,
    'NORMALIZE' = 3,
    'MODIFY_DATA' = 4,
    'PLATE_READY' = 5
  )
}

#' @rdname definestatus
#' @return status
#' @export
define_status.96well_plate <- function(plate) {
  list(
    'NO_DATA' = 0,
    'DATA_INITIALIZED' = 1,
    'FORMAT_DATA' = 2,
    'NORMALIZE' = 3,
    'MODIFY_DATA' = 4,
    'PLATE_READY' = 5
  )
}

#' @rdname definestatus
#' @return status
#' @export
define_status.384well_plate <- function(plate) {
  list(
    'NO_DATA' = 0,
    'DATA_INITIALIZED' = 1,
    'FORMAT_DATA' = 2,
    'NORMALIZE' = 3,
    'MODIFY_DATA' = 4,
    'PLATE_READY' = 5
  )
}

#' @rdname definestatus
#' @return status
#' @export
define_status.1536well_plate_t1 <- function(plate) {
  list(
    'NO_DATA' = 0,
    'DATA_INITIALIZED' = 1,
    'FORMAT_DATA' = 2,
    'NORMALIZE' = 3,
    'MODIFY_DATA' = 4,
    'PLATE_READY' = 5
  )
}

#' @rdname definestatus
#' @return status
#' @export
define_status.1536well_plate_t2 <- function(plate) {
  list(
    'NO_DATA' = 0,
    'DATA_INITIALIZED' = 1,
    'FORMAT_DATA' = 2,
    'NORMALIZE' = 3,
    'MODIFY_DATA' = 4,
    'PLATE_READY' = 5
  )
}

#' @rdname definestatus
#' @return plate
#' @export
set_default_status <- function(plate) {
  type <- class(plate)[1]
  stopifnot(plate %>% inherits(type))
  status(plate) <- define_status(plate)
  plate
}

#' @rdname definestatus
#' @return plate
#' @export
update_status_list <- function(plate) {
  updated_status_list <- list()

  steps_list = steps(plate)
  status_list = define_status(plate)

  for (name in names(status_list)) {
    if (name %in% names(steps_list)) {
      updated_status_list[[name]] <- status_list[[name]]
    }
  }
  plate[['status']] <- updated_status_list
  step_name = get_step_key_by_index(steps(plate), 1)
  status(plate) <- plate[['status']][[step_name]]
  plate
}

#' @rdname definestatus
#' @return plate
#' @export
get_status_value <- function(plate, index){
  step_name = get_step_key_by_index(steps(plate), index)
  status(plate) <- plate[['status']][[step_name]]
  plate
}

#' Status
#' @family status
#' @param plate plate
#' @param value value
#' @return plate
#' @name status
#' @examples
#' \dontrun{status(plate, status)}
NULL

#' @rdname status
#' @return plate
#' @export
status = function(plate){
  type <- class(plate)[1]
  stopifnot(plate %>% inherits(type))
  plate[['status']]
}

#' @rdname status
#' @return plate
#' @export
`status<-` <- function(plate, value) {
  type <- class(plate)[1]
  stopifnot(plate %>% inherits(type))
  plate[['status']] <- value
  plate
}

#' @rdname status
#' @return plate
#' @export
dirty = function(plate){
  type <- class(plate)[1]
  stopifnot(plate %>% inherits(type))
  plate[['dirty']]
}

#' @rdname status
#' @return plate
#' @export
`dirty<-` <- function(plate, value) {
  type <- class(plate)[1]
  stopifnot(plate %>% inherits(type))
  plate[['dirty']] <- value
  plate
}

#' @rdname status
#' @return logical
#' @export
is_plate_dirty <- function(plate) {
  if (dirty(plate) == TRUE){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
