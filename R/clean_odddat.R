#' Title: A function to partially clean dat files from FLuostar experiments
#' @description
#' The function takes a a dirty data frame and returns a clean data frame with the only exception being that most values in the returned data frame have commas at the end
#' The data frame that goes into the function comes from reading dat files from FLUOstar experiments
#' See the example code
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#' @import dplyr
#'
#' @return A new data frame with most impurities taken out; See the example
#' @export
#' @note This function should work on all types of Fluostar dat files (unlike the clean_evendat() function).
#'
#' @examples fpath1 <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' fpath2 <- system.file("extdata", "dat_2.dat", package = "normfluodbf", mustWork = TRUE)
#' dat1_df <- read.table(file=fpath1)
#' dat2_df <- read.table(file=fpath2)
#' partial_cleaned_dat1 <- clean_odddat(dat1_df)
#' partial_cleaned_dat1 <- clean_odddat(dat1_df)
clean_odddat <- function(df){
  special_chars <- c('-,','-' )
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(special_chars[1] %in% df[i,] && special_chars[2] %in% df[i,]){
        df[i,j] <- NA
      }
    }
  }
  nona_rows_df <- na.omit(df)

  for (i in 1:nrow(nona_rows_df)){
    for (j in 1:ncol(nona_rows_df)){
      if(special_chars[1] %in% nona_rows_df[,j] || special_chars[2] %in% nona_rows_df[,j]){
        nona_rows_df[i,j] <- NA
      }
    }
  }

  comma_df <- nona_rows_df %>% select_if(~ !any(is.na(.)))

  return(comma_df)
}
