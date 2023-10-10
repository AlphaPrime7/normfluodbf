#' Title: A scaling up of the resample_dat() function
#' @description
#' Performs the role of the prototype resample_dat() function but it is scaled up to work on all columns in longer-form data frame.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame with n number of rows
#' @param tnp Stands for test,negative,positive (sample types); the number should match the number of sample types in the plate reader even if repeating a sample type
#' @param cycles The number of cycles chosen by the researcher. In the case of this package 40 is the standard but ensure to have the right number of samples
#'
#' @import data.table
#'
#' @return A new data frame with attributes matching the number of sample types and tuples matching the number of cycles. In short it returns delineated samples.
#' @export
#' @note This function is the pre-requisite to the parent or main function of the update.
#' As a matter of fact, this function is modified to produce the parent or main function.
#' @seealso [resample_dat()], [resample_datv2()] and [dat_col_names()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
resample_dat_scale <- function(df, tnp, cycles){

  col_list <- c()
  for(i in 1:ncol(df)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(df[,i])) )
  }

  j_vect <- c()
  for(j in col_list){
    j <- as.data.frame(j)
    j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
    j_dfs <- as.data.frame(j_resampled)
    j_vect <- c(j_vect, j_dfs)
  }

  big_data = do.call(rbind, j_vect)
  big_data = as.data.frame(big_data)
  big_data_t = transpose(l=big_data)

  suppressWarnings(return(big_data_t))
}
