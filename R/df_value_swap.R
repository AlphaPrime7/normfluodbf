#' Title: A function for data frame value replacement
#' @description
#' Swaps values at a specific location with an input replacement value
#'
#' @param df A clean data frame
#' @param row_num row number for replacement value insertion
#' @param col_num column number for replacement value insertion
#' @param replacement_value value used to replace original df value at chosen location
#'
#'
#' @return Return a data frame with a swapped value.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' nofunions <- df_value_swap(resampled_scaled, 2, 2, 2^16)

df_value_swap <- function(df, row_num, col_num, replacement_value){
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){
      if(i == row_num && j ==col_num){
        df[i,j] <- replacement_value
      }
    }
  }
  return(df)
}
