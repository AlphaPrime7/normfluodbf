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
#' @param output A choice between "df' and 'vector' outputs. Leave NULL for a data frame.
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @seealso [resample_vect_scale()]
#'
#' @note
#' This is the vectorized approach and should be a more efficient function when compared to say
#' @seealso [resample_dat()] or @seealso [resample_dat_alt()].
#' This function will produce a horizontal layout as defined in this package.
#'
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' samples_delineated <- resample_dat_vect(nocomma_dat, tnp=3, cycles=40)

resample_dat_vect <- function(df, tnp, cycles, output=NULL){

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

  if(is.null(output)|| output == 'df' ){

    resulting_df = as.data.frame(resulting_vec)
    #colnames(resulting_df) = rep('V', times = ncol(resulting_df))
    colnames(resulting_df) = c(1:ncol(resulting_df))
    return(resulting_df)

  } else {

    return(resulting_vec)
  }
}

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
#' @param method A string 'normal' or 'brute' to specify the method of resampling.
#'
#' @importFrom data.table transpose
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @seealso [resample_dat_vect()]
#'
#' @note
#' This is the pseudo-vectorized approach and should be a more efficient function.
#' This function will produce a horizontal layout as defined in this package.
#' This function inspired by the lapply approach pretty much applies the
#' @seealso [resample_dat_vect()]. As a matter of fact, I took this approach to
#' create compatibility with lapply and rapply but that failed.
#'
#' @examples fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'brute')
#' alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'normal')

resample_vect_scale <- function(df, tnp, cycles, method = c('normal','brute')){

  #df <- df[,colSums(is.na(df))<nrow(df)]
  df_vector = as.vector(df)

  total_list = c()
  for (i in (1:length(df_vector)) ){
    vec_len = length(df_vector[[i]])
    total_list = c(total_list, i)
  }
  vec_len
  total_list

  resulting_df = matrix()
  j_vect <- c()
  dfbs = data.frame() #yes bs= bullshit

  if( is.null(method) || 'normal' %in% method){
    for(j in df_vector){
      j = as.data.frame(j)
      resampled_df = resample_dat_vect(j,tnp=tnp,cycles = cycles)
      colnames(resampled_df) = 'NULL'
      resulting_df = cbind(resulting_df, resampled_df)
    }

    resulting_df = resulting_df[,-1]
    resulting_df = as.data.frame(resulting_df)
    colnames(resulting_df) = c(1:ncol(resulting_df))
    return(resulting_df)

  } else {

    for(k in df_vector){
      k = as.data.frame(k)
      resampled_df = resample_dat_vect(k,tnp=tnp,cycles = cycles)
      j_vect <- c(j_vect, resampled_df)
    }
    dfbs = do.call(rbind, j_vect)
    dfbs = as.data.frame(dfbs)
    dfbs = data.table::transpose(l = dfbs)
    dfbs = as.data.frame(dfbs)
    return(dfbs)
  }

}

