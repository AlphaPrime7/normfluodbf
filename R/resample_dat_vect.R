#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' Designed as a prototype function to take a single attribute or column
#' consisting of several samples and perform a putative resampling
#' to yield another data frame with a separate attribute for each sample.
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
#' @seealso [resample_dat()], [resample_dat_alt()]
#'
#' @note
#' This is the vectorized approach and should be a more efficient function when compared to say
#' @seealso [resample_dat()] or @seealso [resample_dat_alt()].
#' This function will produce a horizontal layout as defined in this package.
#'
#' @examples fpath <- system.file("extdata", "dat_5.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' samples_delineated <- resample_dat_vect(nocomma_dat, tnp=3, cycles=40)

resample_dat_vect <- function(df, tnp, cycles){

  df_vector = as.vector(df)

  for (i in (1:length(df_vector)) ){
    list_len = i
    vec_len = length(df_vector[[i]])
  }
  vec_len
  list_len

  kth = 1
  k = df_vector[[list_len]] %>% .[c(kth)]

  #resulting_vec[1:tnp] <- c(list(), 'NULL')
  resulting_vec <- vector(mode = 'list', length = tnp * ncol(df))

  for(j in 1:(vec_len/tnp)){

    for (l in 1:length(resulting_vec)){

      inc_kth_by = 1

      k = df_vector[[list_len]] %>% .[c(kth)]
      resulting_vec[[l]] = c(resulting_vec[[l]], k)

      kth = kth + inc_kth_by

    }

  }

  return(resulting_vec)
}
