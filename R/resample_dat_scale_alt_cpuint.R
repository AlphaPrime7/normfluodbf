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
#' @seealso [resample_dat_alt()], [resample_dat_scale_alt()]
#'
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_alt_cpuint(nocomma_dat, tnp=3, cycles=40)

resample_dat_scale_alt_cpuint <- function(df, tnp, cycles){

  suppressWarnings({

    col_list <- c()
    for(i in 1:ncol(df)){
      n <- "a"
      col_list <- c(col_list,assign(paste0(n, i), as.data.frame(df[,i])) )
    }

    j_vect <- c()
    for(j in col_list){
      j <- as.data.frame(j)
      j_resampled <- resample_dat_alt(j, tnp = tnp, cycles = cycles)
      j_dfs <- as.data.frame(j_resampled)
      j_vect <- c(j_vect, j_dfs)
    }

    big_data = do.call(rbind, j_vect)
    big_data = as.data.frame(big_data)
    big_data_t = transpose(l=big_data)

    big_data_t <- big_data_t %>% dplyr::select_if(~ !any(is.na(.)))
    #big_data_t <- big_data_t[ , colSums(is.na(big_data_t))==0]

    return(big_data_t)

  })

}
