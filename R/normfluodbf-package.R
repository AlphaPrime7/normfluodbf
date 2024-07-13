## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' @keywords internal
#' @aliases normfluodbf-package
#' @import badger
#' @import magrittr
#' @import dplyr
"_PACKAGE"
utils::globalVariables(
  names = c(
    "%>%",
    "%<>%",
    ".",
    "pkg_globals_cache",
    ".cache",
    ".globals",
    ".normfluodbf_data",
    "plate_types",
    badger::badge_custom("Tingwei", "Adeck", "green", "https://github.com/AlphaPrime7")
  ),
  package = 'normfluodbf',
  add = FALSE
)
NULL

#' Cache
#' @rdname Cache
#' @return NULL
#' @export
pkg_globals_cache <- function() {
  .cache <- environment()
  list(
    get = function(y) .cache[[y]],
    set = function(y, v) .cache[[y]] <- v
  )
}

#' Cache V2
#' @rdname Cache
#' @return NULL
#' @keywords internal
.cache <- function() {
  .store <- new.env()
  list(
    get = function(y) .store[[y]],
    set = function(y, v) .store[[y]] <- v
  )
}

#' Globals Cache
#' @name globalcache
NULL
.globals <- .cache()

#' Internal Troubleshoot
#' @param df data frame
#' @param pkg search value
#' @param name name
#' @param expr expr
#' @return NULL
#' @name internals
NULL

#' @rdname internals
#' @return NULL
#' @export
normfluodbf_data = function(){
  data(package = "normfluodbf")
}

#' @rdname internals
#' @return NULL
#' @export
inspect_workspace = function(){
  utils::ls.str()
}

#' @rdname internals
#' @return NULL
#' @export
get_package_data <- function(name){
  utils::data(name)
  attach(name)
}

#' @rdname internals
#' @return NULL
#' @export
check_cols_class = function(df){
  sapply(df, class)
}

#' @rdname internals
#' @return NULL
#' @export
search_pkg = function(pkg){
  pkgsearch::pkg_search(pkg)
}

#' @rdname internals
#' @return x
#' @export
time_it <- function(expr){
  x = system.time(expr)
  x
}

#' Plate Types Global
#' @name plate_types
NULL
plate_types <- list()
plate_types[['normfluodbf_plate']] <- "normfluodbf_plate"
plate_types[['96well_plate']] <- "96well_plate"
plate_types[['384well_plate']] <- "384well_plate"
plate_types[['1536well_plate_t1']] <- "1536well_plate_t1"
plate_types[['1536well_plate_t2']] <- "1536well_plate_t2"


