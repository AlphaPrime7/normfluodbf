## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Title: Source files
#' @param path path
#' @return All functions from the R file
#' @keywords internal
#' @examples \dontrun{source_files('R')}
source_files <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  sapply(files, source)
}

