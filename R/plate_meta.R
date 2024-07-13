## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plate Meta
#' @family platemeta
#' @param plate plate
#' @param num_wells number of wells
#' @return plate
#' @name platemeta
#' @examples
#' \dontrun{plate_meta(plate, num_wells = 96L)}
#'
NULL

#' @rdname platemeta
#' @return plate
#' @export
plate_meta <- function(plate, num_wells) {
  UseMethod("plate_meta")
}

#' @rdname platemeta
#' @return plate
#' @export
plate_meta.default <- function(plate, num_wells = 96L){

  if (class(plate)[1] != "96well_plate" || class(plate)[1] != "normfluodbf_plate"){
    rlang::abort(sprintf("The type %s is not the right type.", class(plate)[1]))
  }

  well_row = LETTERS[1:8]
  well_col = 1:12

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    tibble::as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}

#' @rdname platemeta
#' @return plate
#' @export
plate_meta.96well_plate <- function(plate, num_wells = 96L){

  if (class(plate)[1] != "96well_plate"){
    rlang::abort(sprintf("The type %s is not the right type.", class(plate)[1]))
  }

  well_row = LETTERS[1:8]
  well_col = 1:12

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    tibble::as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}

#' @rdname platemeta
#' @return plate
#' @export
plate_meta.384well_plate <- function(plate, num_wells = 384L){

  if (class(plate)[1] != "384well_plate"){
    rlang::abort(sprintf("The type %s is not the right type.", class(plate)[1]))
  }

  well_row = LETTERS[1:16]
  well_col = 1:24

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    tibble::as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}

#' @rdname platemeta
#' @return plate
#' @export
plate_meta.1536well_plate_t1 <- function(plate, num_wells = 1536L){

  if (class(plate)[1] != "1536well_plate_t1"){
    rlang::abort(sprintf("The type %s is not the right type.", class(plate)[1]))
  }

  well_row = paste0(rep(LETTERS[1:8], each = 4), letters[1:4])
  well_col = 1:48

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    tibble::as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}

#' @rdname platemeta
#' @return plate
#' @export
plate_meta.1536well_plate_t2 <- function(plate, num_wells){

  if (class(plate)[1] != "1536well_plate_t2"){
    rlang::abort(sprintf("The type %s is not the right type.", class(plate)[1]))
  }

  well_row = c(LETTERS[1:26], paste0("A", LETTERS[1:6]))
  well_col = 1:48

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    tibble::as_tibble() %>%
    tidyr::unite("well", well_row, well_col,
                 sep = "", remove = FALSE) %>%
    dplyr::mutate(
      sample = NA,
      used = FALSE
    ) %>%
    dplyr::select(c('well','sample', 'well_row','well_col','used'))
}

#' Load Plate Meta
#' @family loadplatemeta
#' @param plate plate
#' @param value metadata
#' @return plate
#' @name loadplatemeta
#' @examples
#' \dontrun{load_plate_meta(plate, meta)}
#'
NULL

#' @rdname loadplatemeta
#' @return plate
#' @export
load_plate_meta = function(plate){
  plate[['plate_meta']]
}

#' @rdname loadplatemeta
#' @return plate
#' @export
`load_plate_meta<-` <- function(plate, value) {
  plate[['plate_meta']] <- value
  plate
}


