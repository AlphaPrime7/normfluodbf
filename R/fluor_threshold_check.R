#' Title: Fluorescence experiment quality control function-Check fluorescence threshold
#' @description
#' Check that fluorescence does not exceed 2^16.
#' Experimental design issues should be investigated at fluorescence levels this high.
#' The inventor of this program made a similar mistake when he began these experiments.
#'
#' @param clean_df A cleaned dat or dbf file
#'
#' @return A polite warning message for the researchers next experimental design and the rows and columns with problem values.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' comma_dat <- clean_odddat(dat_df)
#' nocomma_dat <- comma_cleaner(comma_dat)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' resampled_scaled[1,1] <- 2^16
#
#' fluor_threshold_check(resampled_scaled)

fluor_threshold_check <- function(clean_df){
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if(clean_df[i,j] >= (2^16) && !is.null(clean_df[i,j])){
        print("yikes, one or more of your fluorescence values is greater than 2^16(65536), watchout for very high fluorescence in your next experimental design")
        print(paste('column:', j , 'row:', i))
      }
    }
  }
}
