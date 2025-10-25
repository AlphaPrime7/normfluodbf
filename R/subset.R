## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

# ------------------------ Subset S3 Method --------------------------
#' Subset
#' @family subsetutils
#' @param plate plate
#' @param wells wells
#' @param ... dots
#' @return plate
#' @name subset
#' @examples
#' \dontrun{subset(plate,wells)}
NULL

#' @rdname subset
#' @return plate
#' @export
subset = function(plate,wells,...){
  UseMethod('subset')
}

#' @rdname subset
#' @return plate
#' @export
subset.default = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.mod_normfluodbf_data = function(plate,
                                       wells,
                                       ...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.normfluodbf_plate = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.96well_plate = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.384well_plate = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.1536well_plate_t1 = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

#' @rdname subset
#' @return plate
#' @export
subset.1536well_plate_t2 = function(plate,wells,...){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }

  wells_subset_data = plate[['plate_data']] %>%
    dplyr::filter(well %in% wells)

  class(wells_subset_data) = c("normfluodbf_subset", class(wells_subset_data))
  plate[['subset_data']] <- wells_subset_data
  if(is.null(plate[['subset_data']])) {
    plate[['subset']] = FALSE
  }  else {
    plate[['subset']] = TRUE
  }
  plate
}

# ------------------------ Helpers -----------------------------
#' @rdname subset
#' @return plot object
#' @keywords internal
subset_or_not <- function(plate){
  #&& plate[['subset']] == TRUE
  data = NULL
  if(!is.null(plate[['subset_data']])){
    data = plate[['subset_data']]
    data
  }
  else {
    data = plate[['plate_data']]
    data
  }
  data
}

#' @rdname subset
#' @return plot object
#' @keywords internal
remove_subset_data <- function(plate){
  if(!is.null(plate[['subset_data']])){
    plate[['subset_data']] <- NULL
    plate[['subset']] <- NULL
    plate
  } else {
    plate
  }
}

# ------------------------------ Subset Special Circumstances -----------------------------------
#' @rdname subset
#' @return plate
#' @export
subset_unplated_data <- function(data, wells){
  if ('Cycle_Number' %in% names(data) && !('Time' %in% names(data))){
    wells <- union("Cycle_Number", as.character(wells))
  }
  else if ('Time' %in% names(data) && !('Cycle_Number' %in% names(data))){
    wells <- union("Time", as.character(wells))
  }
  else if ('Time' %in% names(data) && 'Cycle_Number' %in% names(data)){
    wells <- union(c("Time", "Cycle_Number"), as.character(wells))
  }
  subset_wells <- data %>%
    select(all_of(wells))

  return(subset_wells)
}

#' @rdname subset
#' @return data
#' @export
#' @keywords internal
subset_for_layout <- function(plate, wells = NULL){
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  }
  data <- plate[['plate_data']]
  data <- data %>%
    dplyr::mutate(used = ifelse(well %in% wells, TRUE, FALSE))
  plate[['subset_data']] <- data
  plate
}
