#' Title: A function to check for DBF files in a directory
#' @description
#' The function checks for the presence of DBF files in a directory OR demands that the user migrates to one.
#'
#' @author Adopted: Tingwei Adeck
#'
#' @param pathstring A string that denotes the path to a directory preferably with dbf files
#'
#' @return A list of DBF files if present in the pwd OR a caution to find a folder with DBF files
#' @export
#'
#'
#' @examples check_dbf(getwd())

check_dbfs_pwd <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$")) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$")
    return(files_list)
  } else{
    return("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}

