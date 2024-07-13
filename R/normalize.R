## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Normalize
#' @family normalize
#' @param plate plate
#' @param df data frame
#' @return plate
#' @name normalize
#' @examples
#' \dontrun{normalize(plate)
#' normalize(plate)}
NULL

#' @rdname normalize
#' @return plate
#' @export
normalize <- function(plate){
  UseMethod("normalize")
}

#' @rdname normalize
#' @return plate
#' @export
normalize.default = function(plate){
  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']])){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_dataframe(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalize
#' @return plate
#' @export
normalize.96well_plate = function(plate){
  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']])){
    message(sprintf('Aborting step %s, because the data is already normalized', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_dataframe(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalize
#' @return plate
#' @export
normalize.384well_plate = function(plate){
  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']])){
    message(sprintf('Aborting step %s, because the data is already normalized', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_dataframe(plate[['plate_data']])
    #plate[['plate_data']] <- NULL
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalize
#' @return plate
#' @export
normalize.1536well_plate_t1 = function(plate){
  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']])){
    message(sprintf('Aborting step %s, because the data is already normalized', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_dataframe(plate[['plate_data']])
    #plate[['plate_data']] <- NULL
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalize
#' @return plate
#' @export
normalize.1536well_plate_t2 = function(plate){
  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']])){
    message(sprintf('Aborting step %s, because the data is already normalized', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_dataframe(plate[['plate_data']])
    #plate[['plate_data']] <- NULL
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalize
#' @return plate
#' @export
normalize_dataframe <- function(df) {
  normalized_df <- df

  if ("Cycle_Number" %in% names(df) && "Time" %in% names(df)){
    columns_to_normalize <- names(df)[-2]
  } else {
    columns_to_normalize <- names(df)[-1]
  }

  # Normalize each column
  for (col in columns_to_normalize) {
    min_val <- min(df[[col]], na.rm = TRUE)
    max_val <- max(df[[col]], na.rm = TRUE)

    normalized_df[[col]] <- (df[[col]] - min_val) / (max_val - min_val)
  }

  return(normalized_df)
}

#' Is Normalized
#' @param data type
#' @param type type
#' @return boolean
#' @name isnormalized
#' @examples
#' \dontrun{is_normalized(data,type)}
NULL

#' @rdname isnormalized
#' @return boolean
#' @export
is_normalized <- function(data, type = c("min-max", "z-score", "hundred")) {
  type <- match.arg(type)

  if ("Cycle_Number" %in% names(data)){
    data <- data[, -which(names(data) == "Cycle_Number")]
    if ("Time" %in% names(data)){
      data <- data[, -which(names(data) == "Time")]
    }
  }

  if (type == "min-max") {
    is_normalized <- sapply(data, function(column) {
      if (is.numeric(column)) {
        min_val <- min(column, na.rm = TRUE)
        max_val <- max(column, na.rm = TRUE)
        return(min_val >= 0 && max_val <= 1)
      } else {
        return(TRUE)
      }
    })
  } else if (type == "z-score") {
    is_normalized <- sapply(data, function(column) {
      if (is.numeric(column)) {
        mean_val <- mean(column, na.rm = TRUE)
        sd_val <- stats::sd(column, na.rm = TRUE)
        return(abs(mean_val) < 1e-8 && abs(sd_val - 1) < 1e-8)
      } else {
        return(TRUE)
      }
    })
  } else if (type == "hundred") {
    is_normalized <- sapply(data, function(column) {
      if (is.numeric(column)) {
        min_val <- min(column, na.rm = TRUE)
        max_val <- max(column, na.rm = TRUE)
        return(min_val >= 0 && max_val <= 100)
      } else {
        return(TRUE)
      }
    })
  }
  return(all(is_normalized))
}

#' Normalize by Well
#' @family normalizebywell
#' @param plate plate
#' @return plate
#' @name normalizebywell
#' @examples
#' \dontrun{normalize_by_well(plate)}
NULL

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well <- function(plate){
  UseMethod("normalize_by_well")
}

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well.default = function(plate){

  normalize_well_values <- function(data) {
    data <- data %>%
      dplyr::group_by(well) %>%
      dplyr::mutate(
        fluor_values = (fluor_values - min(fluor_values)) / (max(fluor_values) - min(fluor_values))
      ) %>%
      dplyr::ungroup()
  }

  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']]$fluor_values)){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_well_values(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well.96well_plate = function(plate){

  normalize_well_values <- function(data) {
    data <- data %>%
      dplyr::group_by(well) %>%
      dplyr::mutate(
        fluor_values = (fluor_values - min(fluor_values)) / (max(fluor_values) - min(fluor_values))
      ) %>%
      dplyr::ungroup()
    data
  }

  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']]$fluor_values)){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_well_values(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well.384well_plate = function(plate){

  normalize_well_values <- function(data) {
    data <- data %>%
      dplyr::group_by(well) %>%
      dplyr::mutate(
        fluor_values = (fluor_values - min(fluor_values)) / (max(fluor_values) - min(fluor_values))
      ) %>%
      dplyr::ungroup()
  }

  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']]$fluor_values)){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_well_values(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well.1536well_plate_t1 = function(plate){

  normalize_well_values <- function(data) {
    data <- data %>%
      dplyr::group_by(well) %>%
      dplyr::mutate(
        fluor_values = (fluor_values - min(fluor_values)) / (max(fluor_values) - min(fluor_values))
      ) %>%
      dplyr::ungroup()
  }

  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']]$fluor_values)){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_well_values(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}

#' @rdname normalizebywell
#' @return plate
#' @export
normalize_by_well.1536well_plate_t2 = function(plate){

  normalize_well_values <- function(data) {
    data <- data %>%
      dplyr::group_by(well) %>%
      dplyr::mutate(
        fluor_values = (fluor_values - min(fluor_values)) / (max(fluor_values) - min(fluor_values))
      ) %>%
      dplyr::ungroup()
    data
  }

  CURRENT_STEP <- plate %>% step('NORMALIZE')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Normalizing Data')
  if (is_normalized(plate[['plate_data']]$fluor_values)){
    message(sprintf('Aborting step %s', 'NORMALIZE'))
    status(plate) = define_status(plate)[['NORMALIZE']] #length(steps(plate)) - (length(steps(plate)) - 2)
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
  else {
    pd = normalize_well_values(plate[['plate_data']])
    load_plate_data(plate) = pd
    status(plate) = define_status(plate)[['NORMALIZE']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Normalized')
    plate
  }
}
