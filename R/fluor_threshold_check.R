#' Title: Fluorescence experiment quality control function-Check fluorescence threshold
#' @description
#' Check that fluorescence does not exceed 2^16.
#' Experimental design issues should be investigated at fluorescence levels this high.
#' The inventor of this program made a similar mistake when he began these experiments.
#'
#' @param clean_df A cleaned dat or dbf file
#' @param fun Used for boolean expressions
#'
#' @import emojifont
#'
#' @return A polite warning message for the researchers next experimental design and the rows and columns with problem values.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' resampled_scaled <- resampled_scaled[,c(1:4)]
#
#' fluor_threshold_check(resampled_scaled)

fluor_threshold_check <- function(clean_df, fun = NA){

  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  clean_df <- rbind(clean_df, NA)
  clean_df <- cbind(clean_df, NA)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if ( clean_df[i,j] >= (2^15) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel')))
        message(paste("YIKES, value > 2^15, Watch in future experimental designs",'column:', j , 'row:', i))
      } else if ( clean_df[i,j] <= (2^11) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel')))
        message(paste("YIKES, value < 2^11, Watch in future experimental designs",'column:', j , 'row:', i))
      }
    }
  }
}

#' Title: Fluorescence experiment quality control function-Check fluorescence threshold
#' @description
#' Check that fluorescence does not exceed 2^16.
#' Experimental design issues should be investigated at fluorescence levels this high.
#' The inventor of this program made a similar mistake when he began these experiments.
#'
#' @param clean_df A cleaned dat or dbf file
#' @param fun Used for boolean expressions
#'
#' @import emojifont
#'
#' @return A polite warning message for the researchers next experimental design and the rows and columns with problem values.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#
#' fluor_threshold_check_na(nocomma_dat)

fluor_threshold_check_na <- function(clean_df, fun = NA){

  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] <= 2^15 || clean_df[i,j] >= 2^11)  && is.na(clean_df[i,j]) != nofun ){
        #nothing
      } else{
        warning("Crikee, some values in your original data are NA values")
        warning(c(emoji('pig'), emoji('camel')))
        warning(paste(j,i))
      }
    }
  }
}

#' Title: Fluorescence experiment quality control function-Check fluorescence threshold
#' @description
#' Check that fluorescence does not exceed 2^16.
#' Experimental design issues should be investigated at fluorescence levels this high.
#' The inventor of this program made a similar mistake when he began these experiments.
#'
#' @param clean_df A cleaned dat or dbf file
#' @param fun Used for boolean expressions
#'
#' @import emojifont
#'
#' @return A polite warning message for the researchers next experimental design and the rows and columns with problem values.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#
#' fluor_threshold_check_raw(nocomma_dat)

fluor_threshold_check_raw <- function(clean_df, fun = NA){

  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] >= 2^15 || clean_df[i,j] <= 2^11)  && is.na(clean_df[i,j]) != nofun ){
        warning(c(emoji('pig'), emoji('camel')))
        warning("Crikee, some values in your original data violate thresholds")

      } else{
        #nothing
      }
    }
  }

}
