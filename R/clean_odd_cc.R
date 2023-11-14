#' Title: DAT file data frame cleaner.
#'
#' @description
#' The function takes the dirty data frame obtained from reading
#' the FLUOstar DAT file and applies a function called comma_cleaner() to the dirty data frame,
#' which automatically inserts NAs in place of the special characters, and rows with NAs only are removed.
#'
#' @author Tingwei Adeck
#'
#' @import badger
#'
#' @param df A dirty data frame obtained from the FLUOstar DAT file.
#'
#'
#' @return A clean data frame with clean NA values retained.
#'
#' @export
#'
#' @seealso [comma_cleaner()], [clean_odddat_optimus()]
#'
#'
#' @examples
#' fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' cleaned_dat <- clean_odd_cc(dat_df)

clean_odd_cc <- function(df){

  #badger::badge_custom("Tingwei", "Adeck", "green", "https://github.com/AlphaPrime7")
  suppressWarnings({

    df <- comma_cleaner(df)
    df <- df[rowSums(is.na(df)) != ncol(df), ]

    return(df)

  })


}
