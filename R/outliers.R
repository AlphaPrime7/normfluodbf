## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Outliers
#' @family removeoutliers
#' @param plate plate
#' @return plate
#' @name removeoutliers
#' @examples
#' \dontrun{remove_outliers(plate)}
NULL

#' @rdname removeoutliers
#' @return plate
#' @export
remove_outliers <- function(plate){
  UseMethod('remove_outliers')
}

#' @rdname removeoutliers
#' @return plate
#' @note Works on a data frame not in well format.
#' @export
remove_outliers.default <- function(plate){
  data <- plate[['plate_data']]

  data <- data %>%
    dplyr::group_by(well) %>%
    dplyr::filter(!(any(fluor_values < 2^11) || any(fluor_values > 2^15))) %>%
    dplyr::ungroup()

  plate[['plate_data']] <- data
  steps(plate) = plate[['steps']][-1]
  return(plate)
}

#' @rdname removeoutliers
#' @return plate
#' @note Works on a data frame not in well format.
#' @export
remove_outliers.96well_plate <- function(plate){
  data <- plate[['plate_data']]

  data <- data %>%
    dplyr::group_by(well) %>%
    dplyr::filter(!(any(fluor_values < 2^11) || any(fluor_values > 2^15))) %>%
    dplyr::ungroup()

  plate[['plate_data']] <- data
  steps(plate) = plate[['steps']][-1]
  return(plate)
}

#' @rdname removeoutliers
#' @return plate
#' @note Works on a data frame not in well format.
#' @export
remove_outliers.384well_plate <- function(plate){
  data <- plate[['plate_data']]

  data <- data %>%
    dplyr::group_by(well) %>%
    dplyr::filter(!(any(fluor_values < 2^11) || any(fluor_values > 2^15))) %>%
    dplyr::ungroup()

  plate[['plate_data']] <- data
  steps(plate) = plate[['steps']][-1]
  return(plate)
}

#' @rdname removeoutliers
#' @return plate
#' @note Works on a data frame not in well format.
#' @export
remove_outliers.1536well_plate_t1 <- function(plate){
  data <- plate[['plate_data']]

  data <- data %>%
    dplyr::group_by(well) %>%
    dplyr::filter(!(any(fluor_values < 2^11) || any(fluor_values > 2^15))) %>%
    dplyr::ungroup()

  plate[['plate_data']] <- data
  steps(plate) = plate[['steps']][-1]
  return(plate)
}

#' @rdname removeoutliers
#' @return plate
#' @note Works on a data frame not in well format.
#' @export
remove_outliers.1536well_plate_t2 <- function(plate){
  data <- plate[['plate_data']]

  data <- data %>%
    dplyr::group_by(well) %>%
    dplyr::filter(!(any(fluor_values < 2^11) || any(fluor_values > 2^15))) %>%
    dplyr::ungroup()

  plate[['plate_data']] <- data
  steps(plate) = plate[['steps']][-1]
  return(plate)
}

#' Detect Outliers
#' @family detectoutliers
#' @param plate plate
#' @param data data
#' @return data frame
#' @name detectoutliers
#' @examples
#' \dontrun{detect_outliers_time_cn(plate, data)}
NULL

#' @rdname detectoutliers
#' @return data frame
#' @export
detect_outliers_time_cn <- function(plate, data){
  data = data %>%
    dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                  "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                  "sample" = well,
                  "used" = TRUE,
                  outlier = dplyr::case_when(
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ TRUE,
                    (fluor_values < params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT')) ~ TRUE,
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT') | fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ FALSE
                  ),
                  outlier_color = dplyr::case_when(
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','NEG'),
                    (fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','NEG'),
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT') | fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','POS')
                  )) %>%
    dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier", "outlier_color")
}

#' @rdname detectoutliers
#' @return data frame
#' @export
detect_outliers_cn <- function(plate, data){
  data = data %>%
    dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                  "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                  "sample" = well,
                  "used" = TRUE,
                  outlier = dplyr::case_when(
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ TRUE,
                    (fluor_values < params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT')) ~ TRUE,
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT') | fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ FALSE
                  ),
                  outlier_color = dplyr::case_when(
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','NEG'),
                    (fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','NEG'),
                    (fluor_values > params(plate,'THRESHOLDS','CUTOFF_LOWER_LIMIT') | fluor_values < params(plate,'THRESHOLDS','CUTOFF_UPPER_LIMIT')) ~ params(plate,'COLORS','POS')
                  )) %>%
    dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "fluor_values", "outlier", "outlier_color")
}
