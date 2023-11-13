#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' A function that takes tuples or rows consisting of several samples and perform a
#' putative resampling to yield another data frame with a separate attribute for each
#' sample.
#'
#' @author Tingwei Adeck
#'
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @seealso [resample_dat_alt()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' resampled_scaled <- resample_dat_scale_alt(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_alt <- function(df, tnp, cycles){

  suppressWarnings({

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

    return(final_df)

  })

}
