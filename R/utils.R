## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#--------------------- Utils ------------------------
## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Directory Utils
#' @family dirutils
#' @param pathstring path string
#' @param fpath fpath
#' @param fname fname
#' @return directory utils
#' @name dirutils
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' list_dbfs(fpath)
#' list_dats(fpath)
#' is_file(fpath)
#' find_known_liposome_dat_file(fpath, 'dat_1.dat')
#' find_known_liposome_dbf_file(fpath, 'liposomes_218')}
NULL

#' @description A function that facilitates a users' workflow by helping to check for DBFs in a directory.
#' @rdname dirutils
#' @return dbfs
#' @export
list_dbfs <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)
    return(files_list)
  } else{
    rlang::warn("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}

#' @description A function that facilitates a users' workflow by helping to check for DATs in a directory.
#' @rdname dirutils
#' @return dbfs
#' @export
list_dats <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)
    return(files_list)
  } else{
    rlang::warn("No .dat files in pwd. Change the directory to one with .dat files")
  }
}

#' @rdname dirutils
#' @return dbfs
#' @export
is_file <- function(pathstring){
  fun = NA
  nofun = is.na(fun)
  if (missing(pathstring) | is.null(pathstring)) {
    return(FALSE)
  }
  pathstring %<>% as.character
  fileinfo = file.info(pathstring)
  if (fileinfo$isdir == nofun){
    return(FALSE)
    #or use !(fileinfo$isdir) instead of the else statement
  } else {
    return(TRUE)
  }

}

#' @rdname dirutils
#' @return dbfs
#' @export
is_dir <- function(pathstring=NULL) {
  if (missing(pathstring) | is.null(pathstring)) {
    return(FALSE)
  }
  pathstring %<>% as.character
  fileinfo <- file.info(pathstring)
  if (is.na(fileinfo$isdir)) {
    return(FALSE)
  }
  fileinfo$isdir
}

#' @rdname dirutils
#' @return dbfs
#' @export
find_known_liposome_dat_file <- function(fpath,fname){
  if(is.null(fpath)){
    rlang::warn("The user is advised to provide a file path")
    fpath = getwd()
  }
  liposome_file = file.path(fpath, sprintf("%s.dat",fname))
  if(is_file(liposome_file)){
    return(liposome_file)
  } else {
    rlang::warn(sprintf("could not find the desired file in directory; looked for `%s`", liposome_file))
    return(NULL)
  }
}

#' @rdname dirutils
#' @return dbfs
#' @export
find_known_liposome_dbf_file <- function(fpath,fname){
  if(is.null(fpath)){
    rlang::warn("The user is advised to provide a file path")
    fpath = getwd()
  }
  liposome_file = file.path(fpath, sprintf("%s.dbf",fname))
  if(is_file(liposome_file)){
    return(liposome_file)
  } else {
    rlang::warn(sprintf("could not find the desired file in directory; looked for `%s`", liposome_file))
    return(NULL)
  }
}

# ----------------------------- File Names ----------------------------------
#' Get File Name(s)
#' @family getfilename
#' @param dat_file DAT file
#' @param dbf_file DBF file
#' @param dat_files DAT files
#' @return file
#' @name getfilename
#' @examples
#' \dontrun{
#' get_dbf_file_name(dbf_file = "liposomes_218.dbf")
#' get_dat_file_name(dat_file = "dat_1.dat")
#' get_common_dat_names(dat_files = list.files(fpath, pattern = "\\.dat$"))}
NULL

#' @rdname getfilename
#' @return name
#' @export
get_dbf_file_name <- function(dbf_file) {
  DBF_FILE_REGEX <- "^(.*)_([0-9][0-9][0-9]).dbf$"
  DBF_FILE_REGEX_NAME <- "\\1"
  gsub(DBF_FILE_REGEX, DBF_FILE_REGEX_NAME, dbf_file)
}

#' @rdname getfilename
#' @return name
#' @export
get_dat_file_name <- function(dat_file) {
  DAT_FILE_REGEX <- "^(.*)_([0-9]).dat$"
  DAT_FILE_REGEX_NAME <- "\\1_\\2"
  gsub(DAT_FILE_REGEX, DAT_FILE_REGEX_NAME, dat_file)
}

#' @rdname getfilename
#' @return name
#' @export
get_dat_common_name <- function(dat_file) {
  DAT_FILE_REGEX <- "^(.*)_([0-9]).dat$"
  DAT_FILE_REGEX_NAME <- "\\1"
  gsub(DAT_FILE_REGEX, DAT_FILE_REGEX_NAME, dat_file)
}

#' @rdname getfilename
#' @return name
#' @export
get_common_dat_names <- function(dat_files) {
  dat_files_names <- get_dat_common_name(dat_files)

  if (dat_files_names %>% unique %>% length > 1) {
    rlang::warn("DAT files dont share a common pattern: proceed to find the most common name")
  }
  name <-
    table(dat_files_names) %>%
    sort(decreasing = TRUE) %>%
    names %>%
    .[1] %>%
    basename
  name
}

# --------------------------------------- Dev Data ------------------------------------
#' Get Development Data
#' @family dirutils
#' @param gotofile file
#' @return directory data
#' @name sampledata
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' sample_data_dir()
#' sample_data_file(gotofile = NULL)}
NULL

#' @rdname sampledata
#' @import stringr
#' @export
sample_data_dir <- function() {
  fun = NA
  nofun = is.na(fun)
  hopath = system.file("sample_data", "dat", package = "normfluodbf")
  if(nchar(hopath) == 0 || str_length(hopath) == 0 || nzchar(hopath) != nofun){
    hopath = system.file("extdata", package = "normfluodbf", mustWork = TRUE)
  } else{
    hopath
  }
  hopath
}

#' @rdname sampledata
#' @export
sample_data_file <- function(gotofile = NULL) {
  if(is.null(gotofile)){
    data_files <- list_dats(sample_data_dir())
    sample_file <- grep("dat_1", data_files, value = TRUE)
    sample_file
  } else {
    data_files <- check_dats(sample_data_dir())
    sample_file <- grep(gotofile, data_files, value = TRUE)
    sample_file
  }
}

# ------------------------- Operator -----------------------------------
#' The %there% operator
#' @family operators
#' @param dfile file
#' @param dirpath directory
#' @return logical
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' "dat_1.dat" %there% fpath}
"%there%" <- function(dfile,dirpath){
  stopifnot(is.character(dfile), is.character(dirpath))
  patterns_list = c("\\.csv$", "\\.dbf$", "\\.dat$", "\\.R", "\\.rds$")
  dlist = c()
  for (j in patterns_list){
    filelist = list.files(path=dirpath, pattern = j, full.names = FALSE)
    dlist = c(filelist, dlist)
  }
  dlist

  flist = c(dfile)
  for (i in flist){
    if (i %in% dlist){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
}

# ----------------------------- Normfluodbf customized Communication Tools -----------------------

#' Directory Utils
#' @family dirutils
#' @param x dev message
#' @param ... dots
#' @return custom messages
#' @name normfluodbfcomms
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' normfluodbf_warn_msg(x = 'life is cool')
#' normfluodbf_msg_msg(x = 'Do some java script and make GUI apps because you earned it')
#' normfluodbf_stop_msg(x = 'Dont Move Forward')}
NULL

#' @rdname normfluodbfcomms
#' @keywords internal
normfluodbf_warn_msg <- function(x) {
  warning(sprintf("normfluodbf: %s", x), call. = NULL)
}

#' @rdname normfluodbfcomms
#' @keywords internal
normfluodbf_stop_msg <- function(x) {
  stop(sprintf("normfluodbf: %s", x), call. = NULL)
}

#' @rdname normfluodbfcomms
#' @keywords internal
normfluodbf_msg_msg <- function(x) {
  message(sprintf("normfluodbf: %s", x), call. = NULL)
}

#' @note Overwrite with \code{options(normfluodbf.verbose = FALSE)}.
#' Write a message to the user if the `normfluodbf.verbose` option is on.
#' @rdname normfluodbfcomms
#' @keywords internal
msg <- function(...) {
  if(isTRUE(getOption("normfluodbf.verbose", default = interactive()))) {
    message(...)
  }
}

#' Quiet
#' @param expr expression
#' @param suppress_messages logical
#' @param suppress_warnings logical
#' @param suppress_output logical
#' @param all logical
#' @return suppress comms
#' @export
#' @examples \dontrun(quiet(expr))
quiet <- function(expr, suppress_messages = FALSE, suppress_warnings = FALSE, suppress_output = FALSE, all = FALSE){
  if (Sys.info()['sysname'] == "Windows") {
    file <- "NUL"
  } else {
    file <- "/dev/null"
  }

  if (all) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      utils::capture.output(expr, file = file)
    )))
  } else {
    if (suppress_output) {
      expr <- utils::capture.output(expr, file = file)
    }

    if (suppress_warnings) {
      expr <- suppressWarnings(expr)
    }

    if (suppress_messages) {
      expr <- suppressMessages(expr)
    }
    expr
  }
}

# ----------------------------- Easy Development Utils -----------------------

#' Check package or function Usage
#' @param directory directory
#' @param package_name package or string
#' @return use location
#' @export
#' @examples \dontrun(check_package_usage('R','capitalize'))
check_package_usage <- function(directory, package_name) {
  r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)

  usage <- list()

  for (file in r_files) {
    file_content <- readLines(file, warn = FALSE)

    lines_with_package <- grep(paste0("\\b", package_name, "\\b"), file_content, value = TRUE)

    if (length(lines_with_package) > 0) {
      usage[[file]] <- lines_with_package
    }
  }

  if (length(usage) > 0) {
    for (file in names(usage)) {
      cat(paste0("Package '", package_name, "' is used in file: ", file, "\n"))
      cat("Lines:\n")
      for (line in usage[[file]]) {
        cat(line, "\n")
      }
      cat("\n")
    }
  } else {
    cat(paste0("Package '", package_name, "' is not used in any R files in the directory: ", directory, "\n"))
  }
}

#' Test Boilerplate
#' @param file_name file name
#' @return test boilerplate
#' @export
#' @note Solves the inconvenient process of navigating to the tests folder every time
#' @examples \dontrun(test_boilerplate(file_name = "test_remove_shit.R"))
#' @rdname testthatutils
test_boilerplate <- function(file_name = NULL) {
  if (!dir.exists("tests/testthat")) {
    dir.create("tests/testthat", recursive = TRUE)
  }

  test_content <- "
library(testthat)

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
  }

testthat::test_that('test...', {
  expect_false(is.null(''))
  })
  "

  file_path <- file.path("tests/testthat", file_name)
  writeLines(test_content, file.path("tests/testthat", file_name))
  message("Boilerplate test file created at: ", file.path("tests/testthat", file_name))

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file.path("tests/testthat", file_name))
  } else {
    message("RStudio API is not available.")
  }
}


#' Open Testfile
#' @param testfile test file
#' @return open test file
#' @export
#' @examples \dontrun(open_testfile('test_pipeline.R'))
#' @rdname testthatutils
open_testfile <- function(testfile){
  file_path <- file.path("tests/testthat", testfile)

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file.path("tests/testthat", testfile))
  } else {
    message("RStudio API is not available.")
  }
}

#' Replace Word
#' @param file_path path
#' @param old_word old func name
#' @param new_word new func name
#' @return file
#' @note Solves the inconvenient problem of renaming a function correctly and having to manually correct it.
#' @export
#' @examples \dontrun(replace_word_in_file('R/plate_plot.R','plot_fluostar_style', 'plot_in_well'))
replace_word_in_file <- function(file_path, old_word, new_word) {
  if (!file.exists(file_path)) {
    stop("The file does not exist.")
  }
  file_content <- readLines(file_path, warn = FALSE)
  modified_content <- gsub(old_word, new_word, file_content)
  writeLines(modified_content, file_path)
  cat("The word has been replaced successfully.\n")
}

#' Check Broken Packages
#' @family utils
#' @family dev_helpers
#' @return broken packages
#' @export
check_broken_packages <- function(){
  .libPaths() %>%
    purrr::set_names() %>%
    purrr::map(function(lib) {
      .packages(all.available = TRUE, lib.loc = lib) %>%
        purrr::keep(function(pkg) {
          f <- system.file('Meta', 'package.rds', package = pkg, lib.loc = lib)
          tryCatch({readRDS(f); FALSE}, error = function(e) TRUE)
        })
    })
}

# ---------------------- General Others -----------------------------

#' Capitalize
#' @param x well
#' @return capital letter
#' @export
#' @examples \dontrun(capitalize('a1'))
capitalize <- function(x){
  paste(toupper(substring(x,1,1)), substring(x,2),
        sep = "", collapse = " ")
}

#' Capitalize
#' @param plate plate
#' @return capital letter
#' @export
#' @examples \dontrun(average_fluorescence_by_row_cycle(plate))
average_fluorescence_by_row_cycle <- function(plate) {
  data <- plate[['plate_data']]
  data %>%
    group_by(well_row, Cycle_Number) %>%  # Group by well_row and Cycle_Number
    summarise(avg_fluorescence = mean(fluor_values, na.rm = TRUE))  # Calculate average fluorescence
  plate[['avg_plate_data']] <- data
  plate
}

