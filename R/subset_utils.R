## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Subset Utils
#' @family subsetutils
#' @param plate plate
#' @param well_id well id
#' @param well1 well1
#' @param well2 well2
#' @param well_range well range
#' @param rangel rangel
#' @param x x
#' @return NULL
#' @name subsetutils
#' @examples
#' \dontrun{get_single_well(plate,well_id)
#' get_wells_btwn(well1,well2)
#' range_to_endpoints(well_range = ("A01, B02:C04, C07"))
#' range_list_to_vec(rangel)}
NULL

#' @rdname subsetutils
#' @return single well
#' @export
get_single_well <- function(plate, well_id) {
  well_id %<>% toupper
  result <-
    plate_meta(plate) %>%
    dplyr::filter(well == well_id) %>%
    dplyr::select(quote(well))
  result
}

#' @rdname subsetutils
#' @return wells between
#' @export
get_wells_btwn <- function(well1, well2) {

  well1 = toupper(well1)
  well2 = toupper(well2)

  dpm <-
    expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
    magrittr::set_colnames(c("row", "col")) %>%
    dplyr::mutate(
      well = sprintf("%s%1d", row, col)
    ) %>%
    dplyr::select(c("well", "row", "col"))

  w1 = which(dpm$well %in% c(well1))
  w2 = which(dpm$well %in% c(well2))

  dpm = dpm[c(w1:w2),]
  w_btwn = dpm$well
  w_btwn = as.vector(w_btwn)
  w_btwn = as.list(w_btwn)

  return(w_btwn)
}

#' @rdname subsetutils
#' @return wells between
#' @keywords internal
.get_wells_btwn <- function(well1, well2) {

  well1 = toupper(well1)
  well2 = toupper(well2)

  rows <-
    substring(c(well1,well2), 1, 1) %>%
    magrittr::is_in(LETTERS, .) %>% which
  rows_seq = seq(min(rows), max(rows)) %>%
    LETTERS[.]
  rows = rows_seq

  cols <-
    substring(c(well1, well2), 2, 3) %>%
    as.integer
  cols_seq = seq(min(cols), max(cols))
  cols = sprintf("%1d", cols_seq)
  cols = as.integer(cols)

  wells <- lapply(rows, function(x) paste(x, cols, sep = "")) %>% unlist
  wells
}

#' @rdname subsetutils
#' @return endpoints
#' @export
range_to_endpoints = function(well_range){
  WELL_ID_REGEX <- "^[A-H]([0-9])?[0-9]$"
  #pattern <- "[A-H]([0-9])?[0-9]:[A-H]([0-9])?[0-9]"
  #s = gsub('(\\w+).*:(\\w+).*', '\\1', well_range)
  #e = gsub('(\\w+).*:(\\w+).*', '\\2', well_range)
  wr <- gsub("[[:space:]]", "", well_range)
  wr <- strsplit(wr, ",") %>% unlist %>% .[. != ""]
  endpoints <- strsplit(wr, ":") %>% unlist
  endpoints <- c(endpoints[1], endpoints[length(endpoints)])
  endpoints <- remove_leading_zero(endpoints)

  if (!grepl(WELL_ID_REGEX, endpoints) %>% all) {
    stop("Invalid wells given to for endpoints")
  }
  if (endpoints %>% length != 2) {
    stop("Invalid range given to endpoints")
  }
  endpoints
}

#' @rdname subsetutils
#' @return wells
#' @export
range_list_to_vec <- function(rangel) {
  rangel <- gsub("[[:space:]]", "", rangel)
  ranges <- strsplit(rangel, ",") %>% unlist %>% .[. != ""]
  if (length(ranges) == 0) {
    return(NULL)
  }

  wells <-
    lapply(ranges, function(range) {
      endpoints <- range_to_endpoints(range)
      get_wells_btwn(endpoints[1], endpoints[length(endpoints)])
    }) %>%
    unlist %>%
    unique %>%
    sort
  wells
}

#' @rdname subsetutils
#' @return boolean
#' @export
is_range <- function(x) {
  length(x) == 1 && grepl("[,:]", x)
}
