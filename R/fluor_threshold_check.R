#' Title: Fluorescence experiment quality control function-Check fluorescence threshold
#' @description
#' Check that fluorescence does not exceed 2^16.
#' Experimental design issues should be investigated at fluorescence levels this high.
#' The inventor of this program made a similar mistake when he began these experiments.
#'
#' @param clean_df A cleaned dat or dbf file
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

fluor_threshold_check <- function(clean_df){
  load.emojifont(font = "EmojiOne.ttf")
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] >= 2^15) || (clean_df[i,j] <= 2^11 ) && !is.null(clean_df[i,j]) ){
        message(c(emoji('pig'), emoji('camel')))
        message("YIKES, one or more of your fluorescence values is greater than 2^16(65536) or less than 2^11(2048), watchout for very high fluorescence or very low in your next experimental design")
        message(paste('these values are either too high or low and can lead to NOISE','column:', j , 'row:', i))
      }
    }
  }
}

