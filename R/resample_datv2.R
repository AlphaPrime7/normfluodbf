#' Title: A function to extract sample types from the long-form (120 tuples) data cleaned data frame.
#' @description
#' Designed as a prototype function to take a single column from the cleaned dat data frame
#' and return the n=3 sample types (n can be any number based on the number of sample types used) as separate attributes with n=40 tuples (n can vary based on the number of cycles ran)
#' The function is well thought and accounts for almost any scenario.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame with n number of rows
#' @param tnp Stands for test,negative,positive (sample types); the number should match the number of sample types in the plate reader even if repeating a sample type
#' @param cycles The number of cycles chosen by the researcher. In the case of this package 40 is the standard but ensure to have the right number of samples
#'
#'
#'
#' @return A new data frame with attributes matching the number of sample types and tuples matching the number of cycles. In short it returns delineated samples.
#' @export
#' @note This function is a subordinate function and prototype that turned out very useful.
#' Syntax modifications present for educational and learning purposes on my part-note taking. One parameter has been taken out compared to resample_dat().
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' col1 <- nocomma_dat[,1]
#' col1_80 <- nocomma_dat[c(1:80),1] #just for fun assume 2 samples only
#' col1 <- as.data.frame(col1)
#' col1_80  <- as.data.frame(col1_80 )
#' samples_delineated <- resample_datv2(col1, tnp=3, cycles=40)
#' two_sample_test <- resample_datv2(col1_80, tnp = 2, cycles = 40)
resample_datv2 <- function(df, tnp, cycles){

  type_size <- c(1:tnp)
  k <- c((1-tnp):(tnp-tnp))

  resulting_df <- data.frame()
  for (i in 1:(nrow(df)/tnp)){

    increment = tnp
    k <- k + increment

    colnames(resulting_df) = NULL
    insert_row = df[k,]
    colnames(insert_row) = NULL

    resulting_df[i,type_size] <- rbind(insert_row, resulting_df)


  }
  suppressWarnings(return(resulting_df))
}
