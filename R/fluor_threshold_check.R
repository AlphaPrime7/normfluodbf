#' Title: A fluorescence quantification Quality Control (QC) function.
#'
#' @description
#' A function designed to check that fluorescence values do not exceed the upper limit (2^15 or 32768)
#' OR fall below the lower limit (2^11 or 2048).
#' Fluorescence values that exceed these thresholds are considered noisy
#' and lead to incorrect interpretation of analysis results.
#'
#' @author Tingwei Adeck
#'
#' @param clean_df A cleaned data frame.
#' @param fun A parameter used for Boolean expressions.
#'
#' @import emojifont
#'
#' @return A polite warning message to the data analyst or researcher.
#'
#' @export
#'
#' @seealso [fluor_threshold_check_na()], [fluor_threshold_check_raw()]
#'
#' @note
#' Experimental issues should be investigated at very high or very low fluorescence values.
#' The most common experimental issues arise when ACMA concentrations are out of the tolerated range.
#' Based on my experience, ACMA concentrations between 2 and 5 Micromolar will suffice
#' to get fluorescence values within the tolerance threshold.
#' ACMA concentrations as low as 0.2 Micromolar or as high as 20 Micromolar have proven problematic based on my research experience.
#'
#' A second issue linked to the FLUOstar instrument revolves around setting the right “gain”
#' to ensure the right level of sensitivity in machine readings.
#' A very high “gain” setting results in increased machine sensitivity even
#' at the right ACMA concentrations and vice versa. In short,
#' we want the machine to be primed to read exactly what we feed it, no more, no less.
#'
#' This function provides the attribute(s) and tuple(s) for the values that need investigation.
#'
#' These deductions were obtained from my experimental hiccups and my characterization of the liposome flux assay system.
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' resampled_scaled <- resampled_scaled[,c(1:4)]
#' fluor_threshold_check(resampled_scaled)

fluor_threshold_check <- function(clean_df, fun = NA){

  load.emojifont(font = "EmojiOne.ttf")

  nofun <- is.na(fun)
  clean_df <- rbind(clean_df, NA)
  clean_df <- cbind(clean_df, NA)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if ( clean_df[i,j] >= (2^15) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel'), emoji('lion')))
        message(paste("YIKES, value > 2^15, Watch in future experimental designs",'column:', j , 'row:', i))
      } else if ( clean_df[i,j] <= (2^11) && is.na(clean_df[i,j]) != nofun ){
        message(c(emoji('pig'), emoji('camel')))
        message(paste("YIKES, value < 2^11, Watch in future experimental designs",'column:', j , 'row:', i))
      }
    }
  }
}

#' Title: A missing value (NA) Quality Control (QC) function.
#'
#' @description
#' A function designed to check for missing values in a data frame.
#'
#' @author Tingwei Adeck
#'
#' @param clean_df A cleaned data frame.
#' @param fun A parameter used for Boolean expressions.
#'
#' @import emojifont
#'
#' @return A polite warning message to the data analyst or researcher.
#'
#' @export
#'
#' @seealso [fluor_threshold_check()], [fluor_threshold_check_raw()]
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
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

#' Title: A fluorescence quantification Quality Control (QC) function.
#'
#' @description
#' A function designed to check that fluorescence values do not exceed the upper limit (2^15 or 32768)
#' OR fall below the lower limit (2^11 or 2048).
#' Fluorescence values that exceed these thresholds are considered noisy
#' and lead to incorrect interpretation of analysis results.
#'
#' @author Tingwei Adeck
#'
#' @param clean_df A cleaned data frame.
#' @param fun A parameter used for Boolean expressions.
#'
#' @import emojifont
#'
#' @return A polite warning message to the data analyst or researcher.
#'
#' @export
#'
#' @seealso [fluor_threshold_check()], [fluor_threshold_check_na()]
#'
#' @note This function works on a different type of data frame but in a similar manner to @seealso [fluor_threshold_check()].
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
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
