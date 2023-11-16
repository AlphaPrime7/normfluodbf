#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#'
#' @author Tingwei Adeck
#'
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param na_omit Takes a string "yes" OR "no".
#'
#' @import stats
#' @importFrom data.table transpose
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is more optimized than @seealso [resample_dat_scale()], hence the suffix scale_optimus.
#'
#' @seealso [resample_dat()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_optimus(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_optimus <- function(df, tnp, cycles, na_omit = NULL){

  suppressWarnings({

    type_size <- c(1:tnp) #k is now nseq(same kinda thing)

    sample_df <- data.frame()
    final_df <- matrix(ncol = cycles)

    for(i in 1:ncol(df)){
      nseq <- c(1:tnp)

      for (j in 1:(nrow(df)/tnp)){

        insert_row = df[nseq,i]
        sample_df[j,type_size] <- rbind(insert_row, sample_df)
        increment_n = tnp
        nseq <- nseq + increment_n

      }
      if(is.null(na_omit) || na_omit == 'no'){
        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(!is.na(final_df))>=1]
        #rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
        colnames(final_df) <- NULL

      } else if(!is.null(na_omit) || na_omit == 'yes'){
        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(is.na(final_df))==0]
        colnames(final_df) <- NULL
        #rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
      }

    }

    return(final_df)
  })

}

#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' NA values are retained.
#'
#' @author Tingwei Adeck
#'
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#'
#' @import stats
#' @importFrom data.table transpose
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is more optimized than @seealso [resample_dat_scale()], hence the suffix scale_optimus.
#'
#' @seealso [resample_dat()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_optimus_na(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_optimus_na <- function(df, tnp, cycles){

  suppressWarnings({
    type_size <- c(1:tnp) #k is now nseq(same kinda thing)


    sample_df <- data.frame()
    final_df <- matrix(ncol = cycles)

    for(i in 1:ncol(df)){
      nseq <- c(1:tnp)

      for (j in 1:(nrow(df)/tnp)){

        insert_row = df[nseq,i]
        sample_df[j,type_size] <- rbind(insert_row, sample_df, na.omit=FALSE)
        increment_n = tnp
        nseq <- nseq + increment_n

      }
      final_df <- cbind(final_df, sample_df)
      final_df <- final_df[ , colSums(!is.na(final_df))>=1]
      #rownames(final_df) <- c(1:cycles)
      final_df <- as.data.frame(final_df)
      colnames(final_df) <- NULL

    }
    #return(sample_df)
    return(final_df)

  })

}
