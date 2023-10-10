#' Title: A function to obtain attribute names for experimental samples
#' @description
#' The function takes a clean data frame, data on the experiment and returns the column names that match the FLUOstar plate reader.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame obtained from the large scale delineation of samples.
#' @param rows_used A character vector representing the plate rows used; eg ru <- c('A','B','C')
#' @param cols_used A numeric vector representing the plate columns used; eg cu <- c(1,2,3,4). keep as null if all the columns were used.
#'
#'
#' @return Returns column names that will be added to the normalized data frame that contains all samples
#' @export
#' @note This function is a subordinate function and follows a sequence of actions. In this package, this function cannot be used as a standalone.
#' Also, some work is needed here on the part of the user because i have no access to their setup file.
#' A function that takes the setup excel file from the user should be part of the next update to prevent the user from doing much work.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' nocomma_dat <- as.data.frame(nocomma_dat)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' n = c('A','B','C')
#' sample_col_names <- dat_col_names(resampled_scaled, n , cols_used = NULL) # i used all columns so col_used = NULL
dat_col_names <- function(df, rows_used = NULL, cols_used= NULL){
  col_names <- c()
  if(is.null(cols_used)){
    for(i in 1:ncol(df)){
      col_names <- c(col_names, paste0(rows_used,i)) #j was n
    }
    return(col_names[1:ncol(df)])
  } else {
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    return(col_names[1:(length(cols_used)*length(rows_used))])
  }
}
