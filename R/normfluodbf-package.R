## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' @keywords internal
#' @aliases normfluodbf-package
#' @import badger
#' @import hexSticker
#' @import magrittr
#' @import dplyr
#' @import shiny
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import plotly
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_false
#' @importFrom testthat test_that
#' @importFrom testthat test_check
"_PACKAGE"
utils::globalVariables(
  names = c(
    "%>%",
    "%<>%",
    ".",
    "::",
    "pkg_globals_cache",
    ".cache",
    ".globals",
    ".normfluodbf_data",
    "plate_types",
    badger::badge_custom("Tingwei", "Adeck", "green", "https://github.com/AlphaPrime7"),
    "legend",
    "well",
    "well_row",
    "Cycle_Number",
    "fluor_values",
    "well_col",
    "read_rds",
    "used",
    "sd",
    "setNames",
    "labeller",
    "theme",
    "element_text",
    "element_rect",
    "legend",
    "check_dats",
    "write_rds",
    "dbfs",
    "read_dbf",
    "drop_na"
  ),
  package = 'normfluodbf',
  add = FALSE
)
NULL

#' Cache
#' @rdname Cache
#' @return cache
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
#' @return env
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
#' @return internals
#' @examples \dontrun{
#' get_package_data('iris')}
#' @name internals
NULL

#' @rdname internals
#' @return all data
#' @export
normfluodbf_data = function(){
  data(package = "normfluodbf")
}

#' @rdname internals
#' @return workspace
#' @keywords internal
inspect_workspace = function(){
  x <- ls(envir = .GlobalEnv)
  y <- mget(x, envir = .GlobalEnv)
  y
}

#' @rdname internals
#' @return pkg data
#' @keywords internal
get_package_data <- function(name){
  eval(parse(text = paste("data(", name, ")", sep = "")))
  x <- get(name)
  x
}

#' @rdname internals
#' @return classes
#' @export
check_cols_class = function(df){
  sapply(df, class)
}

#' @rdname internals
#' @return package
#' @export
search_pkg = function(pkg){
  pkgsearch::pkg_search(pkg)
}

#' @rdname internals
#' @return duration
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
