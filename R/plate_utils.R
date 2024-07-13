## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Save and Load Plate
#' @family plate_utils
#' @param plate plate
#' @param suffix suffix
#' @param interactive boolean
#' @param save_name name
#' @param use_tempfile boolean
#' @param var variable
#' @return NULL
#' @name saveloadutils
NULL

#' @rdname saveloadutils
#' @return plate
#' @export
save_plate = function(plate, suffix=NULL, interactive = F){
  varstr = var_str(plate)
  save_name = paste(varstr, '_' ,suffix ,'.RData',sep = "") #gsub(" ", "", paste(plate,'.RData'))
  save(plate, file = save_name)
}

#' @rdname saveloadutils
#' @return plate
#' @export
save_rds_plate = function(plate, save_name, use_tempfile = F){

  if (use_tempfile == F){
    write_rds(x = plate,
              file = paste(save_name,'.rds',sep = ""),
              compress = c("none", "gz", "bz2", "xz"),
              version = 2)
  } else {
    temp <- tempfile()
    write_rds(x = plate,
              file = paste(temp,'.rds',sep = ""),
              compress = c("none", "gz", "bz2", "xz"),
              version = 2)
    sprintf("Use load_rds_plate(%s) for loading the plate",'temp')
  }
}

#' @rdname saveloadutils
#' @return plate
#' @export
load_rds_plate = function(plate, interactive = F){

  if (interactive == F && !is.null(plate)){
    read_rds(plate)
  }
  else {
    path = file.choose()
    read_rds(path)
  }
}

#' @rdname saveloadutils
#' @return plate
#' @export
var_str <- function(var) {
  x = deparse(substitute(var))
  return(x)
}

#' Plate Version
#' @family plate_utils
#' @param plate plate
#' @param pkg package
#' @return plate
#' @export
#' @examples \dontrun{set_plate_version(plate,pkg)}
set_plate_version = function(plate,pkg){
  plate[['version']] <- as.character(utils::packageVersion(pkg))
  plate
}

#' Plate Type
#' @family plate_utils
#' @param plate plate
#' @param all Boolean
#' @return class attribute
#' @export
#' @examples
#' \dontrun{type(plate)}
type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}

#' Check Dirt
#' @family plate_utils
#' @param plate plate
#' @return plate
#' @export
check_dirt = function(plate){
  if (is.null(plate[['plate_data']]) ){
    plate[['dirty']] <- FALSE
  } else {
    plate[['dirty']] <- TRUE
  }
  plate
}

#' Plate Name
#' @family plate_utils
#' @param plate plate
#' @param value value
#' @return NULL
#' @name platename
NULL

#' @rdname platename
#' @return plate
#' @export
name = function(plate){
  stopifnot(plate %>% inherits('normfluodbf_plate'))
  plate[['name']]
}

#' @rdname platename
#' @return plate
#' @export
`name<-` <- function(plate, value) {
  stopifnot(plate %>% inherits('normfluodbf_plate'))
  plate[['name']] = value
  plate
}

#' Plate Data Summary
#' @family plate_utils
#' @param plate plate
#' @return sprintf string
#' @export
plate_data_summary <- function(plate){
  used_data = dplyr::filter(plate[['plate_data']],used)
  sample_cols = used_data %>%
    .[['well']] %>%
    as.vector() %>%
    unique()
  return(sprintf('%d Wells Used', length(sample_cols)))
}

#' Format Well Names
#' @family plate_utils
#' @param names_vector column names
#' @return vector
#' @export
#' @examples \dontrun{remove_leading_zero(names)}
remove_leading_zero <- function(names_vector) {
  gsub("([A-Z])0", "\\1", names_vector)
}

#' Wells Used
#' @family plate_utils
#' @param pl_data data
#' @return wells used
#' @export
#' @examples \dontrun{get_wells_used(data)}
get_wells_used = function(pl_data){
  pat = "^\\w{1-2}\\d+"
  wells_used = names(pl_data)
  wells_used = remove_leading_zero(wells_used)
  wells_used <- grep(pat,wells_used , value = TRUE)
  wells_used = tibble::as_tibble(wells_used) %>%
    magrittr::set_colnames(c('wells_used')) %>%
    dplyr::mutate(
      used = TRUE)
  wells_used
}


