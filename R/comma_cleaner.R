#' Title: A function to remove unwanted commas in a partially cleaned data frame
#' @description
#' The function takes a a dirty data frame (comma_df) derived from either the clean_odddat or clean_evendat functions.
#' The resulting data frame is a cleaned data frame with numeric values.
#'
#' @author Tingwei Adeck
#' @param df A dirty data frame (numeric values have commas at the end) with n number of rows
#'
#'
#' @return A new data frame with commas at the end of numbers taken out
#' @export
#' @note This function is a subordinate function and follows a sequence of actions. In this package, this function cannot be used as a standalone.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat1_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
comma_cleaner <- function(comma_df){
  cols_to_becleaned <- c(colnames(comma_df))
  comma_df[ , cols_to_becleaned] <- lapply(comma_df[ , cols_to_becleaned],  # Convert data
                                           function(x){ as.numeric(as.character(gsub(",", "", x))) })

}
