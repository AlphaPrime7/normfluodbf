## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Analyze
#' @family analyze
#' @param plate plate
#' @return plate
#' @name analyze
#' @examples
#' \dontrun{analyze_ready(plate)}
NULL

#' @rdname analyze
#' @return plate
#' @export
analyze_ready <- function(plate){
  UseMethod("analyze_ready")
}

#' @rdname analyze
#' @return plate
#' @export
analyze_ready.default <- function(plate){
  if (class(plate[['plate_data']])[1] == "mod_normfluodbf_data" && is_normalized(plate[['plate_data']]$fluor_values)){
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
  else {
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
}

#' @rdname analyze
#' @return plate
#' @export
analyze_ready.96well_plate <- function(plate){
  if (class(plate[['plate_data']])[1] == "mod_normfluodbf_data" && is_normalized(plate[['plate_data']]$fluor_values)){
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
  else {
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
}

#' @rdname analyze
#' @return plate
#' @export
analyze_ready.384well_plate <- function(plate){
  if (class(plate[['plate_data']])[1] == "mod_normfluodbf_data" && is_normalized(plate[['plate_data']]$fluor_values)){
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
  else {
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
}

#' @rdname analyze
#' @return plate
#' @export
analyze_ready.1536well_plate_t1 <- function(plate){
  if (class(plate[['plate_data']])[1] == "mod_normfluodbf_data" && is_normalized(plate[['plate_data']]$fluor_values)){
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
  else {
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
}

#' @rdname analyze
#' @return plate
#' @export
analyze_ready.1536well_plate_t2 <- function(plate){
  if (class(plate[['plate_data']])[1] == "mod_normfluodbf_data" && is_normalized(plate[['plate_data']]$fluor_values)){
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
  else {
    status(plate) = define_status(plate)[['PLATE_READY']]
    steps(plate) = plate[['steps']][-1]
    steps(plate) = as.numeric(FALSE)
    plate[['data_summary']] = plate_data_summary(plate)
    plate
  }
}

