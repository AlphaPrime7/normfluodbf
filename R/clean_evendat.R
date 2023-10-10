#' Title: A function to partially clean dat files with an identified pattern from FLuostar experiments
#' @description
#' The function takes a dirty data frame and returns a clean data frame with the only exception being that most values in the returned data frame have commas at the end
#' The data frame that goes into the function comes from reading dat files from FLUOstar experiments
#' See the example code
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#' @return A new data frame with most impurities taken out. See the example
#' @export
#' @note This function only works on a specific type of Fluostar dat file and really serves to be made available for anyone to find a universal utility for the function.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat1_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_evendat(dat1_df)
clean_evendat <- function(df){
  for (i in (4 * (1:(nrow(df)/4)))){
    k <- seq(4)
    skip_values = 8 * seq(40)
    if(i %in% skip_values) next
    df[c(k+i,i),] <- NA
  }
  df <- na.omit(df)
  return(df)
}
