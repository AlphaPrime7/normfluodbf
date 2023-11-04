#' Title: DAT file data frame cleaner.
#'
#' @description
#' The function takes the dirty data frame obtained from reading the FLUOstar DAT file, applies an original
#' algorithm that inserts NAs in place of the special characters, and then applies a function called
#' comma_cleaner() to the dirty data frame for the removal of commas, and finally, rows with NAs only are removed.
#'
#' @author Tingwei Adeck
#'
#' @param df A dirty data frame obtained from the FLUOstar DAT file.
#'
#'
#' @return A clean data frame with clean NA values retained.
#'
#' @export
#'
#' @seealso [comma_cleaner()], [clean_odd_cc()]
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odddat_optimus(dat_df)

clean_odddat_optimus <- function(df){

  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j]){
        df[i,j] <- NA
      }
    }
  }
  na_df <- df

  comma_df <- na_df
  nocomma_df <- comma_cleaner(comma_df)
  nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]

  return(nocomma_df)
}
