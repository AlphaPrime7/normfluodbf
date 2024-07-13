## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Define Plate Parameters
#' @family defineparams
#' @param plate plate
#' @return NULL
#' @name defineparams
#' @examples
#' \dontrun{define_params(plate)}
#'
NULL

#' @rdname defineparams
#' @return NULL
#' @export
define_params = function(plate){
  UseMethod("define_params")
}

#' @rdname defineparams
#' @return default params
#' @export
define_params.default <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  THRESHOLDS <- list()
  THRESHOLDS['MAX_FLUOR'] <- 2^16
  THRESHOLDS['MIN_FLUOR'] <- 2^7
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

#' @rdname defineparams
#' @return default params
#' @export
define_params.96well_plate <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  THRESHOLDS <- list()
  THRESHOLDS['MAX_FLUOR'] <- 2^16
  THRESHOLDS['MIN_FLUOR'] <- 2^7
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

#' @rdname defineparams
#' @return default params
#' @export
define_params.384well_plate <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  THRESHOLDS <- list()
  THRESHOLDS['MAX_FLUOR'] <- 2^16
  THRESHOLDS['MIN_FLUOR'] <- 2^7
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

#' @rdname defineparams
#' @return default params
#' @export
define_params.1536well_plate_t1 <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  THRESHOLDS <- list()
  THRESHOLDS['MAX_FLUOR'] <- 2^16
  THRESHOLDS['MIN_FLUOR'] <- 2^7
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

#' @rdname defineparams
#' @return default params
#' @export
define_params.1536well_plate_t2 <- function(plate) {

  GENERAL <- list()
  GENERAL['X_VAR_ONE'] <- "Time"
  GENERAL['X_VAR_TWO'] <- "Cycle_Number"
  GENERAL['Y_VAR'] <- "fluor_values"
  GENERAL['X_VAR_ONE_LABEL'] <- "TIME"
  GENERAL['X_VAR_TWO_LABEL'] <- "CYCLE NUMBER"
  GENERAL['Y_VAR'] <- "FLUORESCENCE"

  THRESHOLDS <- list()
  THRESHOLDS['MAX_FLUOR'] <- 2^16
  THRESHOLDS['MIN_FLUOR'] <- 2^7
  THRESHOLDS['CUTOFF_UPPER_LIMIT'] <- 2^15 #32768
  THRESHOLDS['CUTOFF_LOWER_LIMIT'] <- 2^12 #4096

  COLORS <- list()
  COLORS['POS'] <- "#7FFFD4"
  COLORS['TEST'] <- "#0000CD"
  COLORS['NEG'] <- "#EE3B3B"
  COLORS['NEUTRAL'] <- "#BF3EFF"
  # #CDC1C5, #FFFAFA, #CDC9C9, #EE9A49, #8B5A2B, #FFA500, #EE4000,

  DEFAULT_PARAMS <- list(
    'GENERAL' = GENERAL,
    'THRESHOLDS' = THRESHOLDS,
    'COLORS' = COLORS
  )
  DEFAULT_PARAMS
}

#' @rdname defineparams
#' @return plate
#' @export
set_default_params <- function(plate) {
  plate[['params']] <- define_params(plate)
  plate
}

#' Set Plate Params
#' @family params
#' @param plate plate
#' @param category category
#' @param name name
#' @return NULL
#' @name setparams
#' @examples
#' \dontrun{params(plate, cat, name)}
#'
NULL

#' @rdname setparams
#' @return plate
#' @export
params = function(plate,category,name){
  plate[['params']][[category]][[name]]
}

#' @rdname setparams
#' @return plate
#' @export
`params<-` = function(plate, category,name, value){
  plate['params'][[category]][[name]] <- value
  plate
}

#' @rdname setparams
#' @return plate
#' @keywords internal
.params = function(plate, category, name){
  stopifnot(plate %>% inherits("normfluodbf_plate"))

  res <- plate[['params']]
  if (!missing(category)) {
    res <- res[[category]]
    if (!missing(name)) {
      res <- res[[name]]
    }
  }
  res
}
