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
#'
#' @import stats
#' @importFrom data.table transpose
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is less optimized than @seealso [resample_dat_scale_optimus()].
#'
#' @seealso [resample_dat()]
#'
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat(df, tnp = tnp, cycles = cycles)
      return(df1)

    } else {

      col_list <- c()
      for(i in 1:ncol(df)){
        col_list <- c( col_list, as.data.frame(df[,i]) )
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
      big_data_t = data.table::transpose(l=big_data)
      big_data_t <- big_data_t %>% dplyr::select_if(~ !any(is.na(.)))
      #big_data_t <- big_data_t[ , colSums(is.na(big_data_t))==0]
      big_data_t <- as.data.frame(big_data_t)

      return(big_data_t)

    }

  })

}

#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' NA values are retained for more control.
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
#' This function is less optimized than @seealso [resample_dat_scale_optimus()].
#'
#' @seealso [resample_dat()]
#'
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_naretainer(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_naretainer <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat(df, tnp, cycles)
      return(df1)

    } else {

      col_list <- c()
      for(i in 1:ncol(df)){
        col_list <- c( col_list, as.data.frame(df[,i]) )
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

      big_data_t <- big_data_t[ , colSums(!is.na(big_data_t))>=1]
      #big_data_t <-  big_data_t[, !sapply(big_data_t, function(x) all(x == 0))]
      #big_data_t <- big_data_t[,which(!apply(big_data_t,2,FUN = function(x){all(x == 0)}))]
      big_data_t <- as.data.frame(big_data_t)

      return(big_data_t)

    }

  })

}
