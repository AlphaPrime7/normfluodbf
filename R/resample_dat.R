#' Title: A function to extract sample types from the 120 tuples data cleaned data frame.
#' @description
#' Designed as a prototype function to take a single column from the cleaned dat data frame
#' and return the n=3 sample types (n can be any number based on the number of sample types used) as separate attributes with n=40 tuples (n can vary based on the number of cycles ran)
#' The function is well thought and accounts for almost any scenario.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame with n number of rows
#' @param tnp Stands for test,negative,positive (sample types); the number should match the number of sample types in the plate reader even if repeating a sample type
#' @param cycles The number of cycles chosen by the researcher. In the case of this package 40 is the standard but ensure to have the right number of samples
#' @param samples_per_tnp An optional parameter thought to be useful but had no use based on the approach taken to solve this problem; Will be useful in future functions
#'
#'
#'
#' @return A new data frame with attributes matching the number of sample types and tuples matching the number of cycles. In short it returns delineated samples.
#' @export
#' @note This function is a subordinate function and prototype that turned out very useful.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat1_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' col1 <- nocomma_dat[,1]
#' col1_80 <- nocomma_dat[c(1:80),1] #just for fun assume 2 samples only
#' samples_delineated <- resample_dat(col1, tnp=3, cycles=40)
#' two_sample_test <- resample_dat(col1_80, tnp = 2, cycles = 40, samples_per_tnp = 12)
resample_dat <- function(df, tnp, cycles, samples_per_tnp=NULL){

  type_size <- c(1:tnp)
  k <- c(1:tnp)

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    colnames(resulting_df) = NULL
    insert_row = df[k,]
    colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)

    increment = tnp
    k <- k + increment


  }
  suppressWarnings(return(resulting_df))
}
