#' Title: A scaling up of the resample_dat() function
#' @description
#' Performs the role of the prototype resample_dat() function but it is scaled up to work on all columns in longer-form data frame.
#'
#' @author Tingwei Adeck
#' @param df A clean data frame with n number of rows
#' @param tnp Stands for test,negative,positive (sample types); the number should match the number of sample types in the plate reader even if repeating a sample type
#' @param cycles The number of cycles chosen by the researcher. In the case of this package 40 is the standard but ensure to have the right number of samples
#'
#'
#' @return A new data frame with attributes matching the number of sample types and tuples matching the number of cycles. In short it returns delineated samples.
#' @export
#' @note This function is the pre-requisite to the parent or main function of the update.
#' As a matter of fact, this function is modified to produce the parent or main function.
#' @seealso [resample_dat()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' resampled_scaled <- resample_dat_scale_alt(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_alt <- function(df, tnp, cycles, samples_per_tnp=NULL){

  type_size <- c(1:ncol(df)) #k is now nseq(same kinda thing)

  sample_df <- data.frame()
  final_df <- matrix(ncol = cycles)

  for(i in 1:tnp){
    nseq <- c(i:ncol(df))
    nseq <- nseq

    for (j in 1:(nrow(df)/tnp)){

      insert_row = df[nseq,]
      sample_df[j,type_size] <- rbind(insert_row, sample_df)
      increment_n = tnp
      nseq <- nseq + increment_n

    }
    final_df <- cbind(final_df, sample_df)
    final_df <- final_df[ , colSums(is.na(final_df))==0]
    colnames(final_df) <- NULL
    rownames(final_df) <- c(1:cycles)
    final_df <- as.data.frame(final_df)

  }
  #return(sample_df)
  return(final_df)
}
