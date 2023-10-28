#' Title: A function to partially clean dat files from FLuostar experiments
#' @description
#' The function takes a dirty data frame and returns a clean data frame with the only exception being that most values in the returned data frame have commas at the end
#' The data frame that goes into the function comes from reading dat files from FLUOstar experiments
#' See the example code
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#'
#' @return A new data frame with most impurities taken out; See the example
#' @export
#' @note This function should work on all types of Fluostar dat files (unlike the clean_evendat() function).
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odddat_optimus(dat_df)

clean_odddat_optimus <- function(df){

  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j]){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- df

  comma_df <- nona_rows_df
  comma_df <- comma_cleaner(comma_df)
  comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]

  return(comma_df)
}

#' Title: A function to partially clean dat files from FLuostar experiments
#' @description
#' The function takes a dirty data frame and returns a clean data frame with the only exception being that most values in the returned data frame have commas at the end
#' The data frame that goes into the function comes from reading dat files from FLUOstar experiments
#' See the example code
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#'
#' @return A new data frame with most impurities taken out; See the example
#' @export
#' @note This function should work on all types of Fluostar dat files (unlike the clean_evendat() function).
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odddat_prime(dat_df)

clean_odddat_prime <- function(df){

  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j]){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- df

  for (i in 1:nrow(nona_rows_df)){
    for (j in 1:ncol(nona_rows_df)){
      if(special_chars[1] %in% nona_rows_df[i,j] || special_chars[2] %in% nona_rows_df[i,j]){
        nona_rows_df[i,j] <- NA
      }
    }
  }

  comma_df <- nona_rows_df
  comma_df <- comma_cleaner(comma_df)
  comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]

  return(comma_df)
}

