#' Title: A function to check for DBFs in a directory.
#'
#' @description
#' A function that facilitates a users' workflow by helping to check for DBFs in a directory.
#'
#' @author Tingwei Adeck
#'
#' @param pathstring A string for a path to a directory containing files.
#'
#' @return Returns a list of DBF files or a warning.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' check_dbf(getwd())
#' }

check_dbf <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)
    return(files_list)
  } else{
    normfluodbf_warn_msg("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}

#' @export
list_dbfs <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$", full.names = TRUE)
    return(files_list)
  } else{
    normfluodbf_warn_msg("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}

#' Title: A function to check for DATs in a directory.
#'
#' @description
#' A function that facilitates a users' workflow by helping to check for DATs in a directory.
#'
#' @author Tingwei Adeck
#'
#' @param pathstring A string for a path to a directory containing files.
#'
#' @return Returns a list of DAT files or a warning.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
#' setwd(fpath)
#' check_dat(getwd())
#' }

check_dat <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)
    return(files_list)
  } else{
    normfluodbf_warn_msg("No .dat files in pwd. Change the directory to one with .dat files")
  }
}

#' @export
list_dats <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dat$", full.names = TRUE)
    return(files_list)
  } else{
    normfluodbf_warn_msg("No .dat files in pwd. Change the directory to one with .dat files")
  }
}

#' File check
#' @example
#' \dontrun{
#' fpath = "C:/Users/..."
#' is_file(fpath)
#' }
#'

is_file <- function(fpath){

  fun = NA
  nofun = is.na(fun)

  if (missing(fpath) | is.null(fpath)) {
    return(FALSE)
  }

  fpath %<>% as.character
  fileinfo = file.info(fpath)
  if (fileinfo$isdir == nofun){
    return(FALSE)
    #or use !(fileinfo$isdir) instead of the else statement
  } else {
    return(TRUE)
  }

}

#' Directory check
#' @example
#' \dontrun{
#' dpath = "C:/Users/..."
#' is_file(dpath)
#' }
#'

is_dir <- function(path=NULL) {
  if (missing(path) | is.null(path)) {
    return(FALSE)
  }
  path %<>% as.character
  fileinfo <- file.info(path)
  if (is.na(fileinfo$isdir)) {
    return(FALSE)
  }
  fileinfo$isdir
}

#' Known file path
#' @example
#' \dontrun{
#' fpath = "C:/Users/..."
#' fname = "test"
#' find_known_liposome_file(fpath, fname)
#' }
#'

find_known_liposome_file <- function(fpath,fname){

  if(is.null(fpath)){
    normfluodbf_msg_msg("The user is advised to provide a file path")
    return(NULL)
  }

  liposome_file = file.path(fpath, sprintf("%s.dat",fname))
  if(is_file(liposome_file)){
    return(liposome_file)
  } else {
    normfluodbf_warn_msg(sprintf("could not find the desired file in directory; looked for `%s`", liposome_file))
    return(NULL)
  }

}


#' %there% operator
#' @note My first R special operator. For now I advise checking a single file name.
#' For multiple files, the program will return TRUE if any file in the list is present.
#' @example
#' \dontrun{
#' dfile = c("test.csv")
#' dirpath = "C:/Users/..."
#' c("test.csv") %there% tpath
#' }

"%there%" <- function(dfile,dirpath){

  stopifnot(is.character(dfile), is.character(dirpath))

  patterns_list = c("\\.csv$", "\\.dbf$", "\\.dat$", "\\.R", "\\.rds$")
  dlist = c()
  for (j in patterns_list){
    filelist = list.files(path=tpath, pattern = j, full.names = FALSE)
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

#' @export
#' @note The filenames were originally performed by the author(me).
#' This is mostly a learning function and placed in the program for future use.
#' This function can also be performed on a list of files using \code{list.files}.
#' @example
#' \dontrun{
#' get_dbf_file_name("liposomes_218.dbf")
#' }

get_dbf_file_name <- function(dbf_file) {
  DBF_FILE_REGEX <- "^(.*)_([0-9][0-9][0-9]).dbf$"
  DBF_FILE_REGEX_NAME <- "\\1"
  gsub(DBF_FILE_REGEX, DBF_FILE_REGEX_NAME, dbf_file)
}

#' @export
#' @note The filenames were originally performed by the author(me).
#' This is mostly a learning function and placed in the program for future use.
#' This function is used in events when the author gets a chance to get access to
#' a FLUOStar instrument.
#' @example
#' \dontrun{
#' get_dat_file_name(""dat_5.dat"")
#' }
#'

get_dat_file_name <- function(dat_file) {
  DAT_FILE_REGEX <- "^(.*)_([0-9]).dat$"
  DAT_FILE_REGEX_NAME <- "\\1_\\2"
  gsub(DAT_FILE_REGEX, DAT_FILE_REGEX_NAME, dat_file)
}

#' @export
#' @note The dat common name is not machine sourced but used in this program for
#' ease of reference/testing.
#' @example
#' \dontrun{
#' get_dat_common_name(""dat_5.dat"")
#' }

get_dat_common_name <- function(dat_file) {
  DAT_FILE_REGEX <- "^(.*)_([0-9]).dat$"
  DAT_FILE_REGEX_NAME <- "\\1"
  gsub(DAT_FILE_REGEX, DAT_FILE_REGEX_NAME, dat_file)
}

#' @export
#' @note If the user is interested in seeing the pattern in which dat files
#' were named for purposes of developing this program.
#' Not suited for casual users (appropriate for fellow developers).
#' For the sake of time, this function is not applied to DBF files.
#' @example
#' \dontrun{
#' get_common_dat_names(list.files(tpath, pattern = "\\.dat$"))
#' }
get_common_dat_names <- function(dat_files) {
  dat_files_names <- get_dat_common_name(dat_files)

  if (dat_files_names %>% unique %>% length > 1) {
    normfluodbf_warn_msg("DAT files dont share a common pattern: proceed to find the most common name")
  }

  name <-
    table(dat_files_names) %>%
    sort(decreasing = TRUE) %>%
    names %>%
    .[1] %>%
    basename

  name
}

#' Get developmental data
#'
#' Load sample data for testing and developmental purposes.
#' @examples
#' sample_data_dir()
#' @name sample_data
NULL

#' @rdname sample_data
#' @import stringr
#' @export

sample_data_dir <- function() {
  fun = NA
  nofun = is.na(fun)
  hopath = system.file("sample_dat_dbf_files", "dat", package = "normfluodbf")
  if(nchar(hopath) == 0 || str_length(hopath) == 0 || nzchar(hopath) != nofun){
    hopath = system.file("extdata", package = "normfluodbf", mustWork = TRUE)
  } else{
    hopath
  }
  hopath
}

#' @rdname sample_data
#' @export
#' @note The developmental goto file when it comes to testing the program.
#' From now on running tests in R for this program should be a breeze.
#' @example
#' sample_data_file()

sample_data_file <- function(gotofile = NULL) {

  if(is.null(gotofile)){
    data_files <- check_dat(sample_data_dir())
    sample_file <- grep("dat_1", data_files, value = TRUE)
    sample_file
  } else {
    data_files <- check_dat(sample_data_dir())
    sample_file <- grep(gotofile, data_files, value = TRUE)
    sample_file
  }

}
