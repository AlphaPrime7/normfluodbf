#' Title: A function to completely clean dat files from FLuostar experiments
#' @description
#' The function takes a dirty data frame and returns a clean data frame.
#' The function takes care of more QC concerns not seen in previous cleaning functions.
#' The function accounts for possible machine data formatting when users skip wells by
#' retaining NA values, which ensures that the user does not loose samples in data analysis.
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#'
#' @return A new data frame with NAs retained.
#' @export
#' @seealso [normfluodat()]
#' @note This function is superior to clean_odddat().
#'
#' @examples
#' fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odd_cc(dat_df)

clean_odd_cc <- function(df){

  df <- comma_cleaner(df)
  df <- df[rowSums(is.na(df)) != ncol(df), ]

  return(df)

}
