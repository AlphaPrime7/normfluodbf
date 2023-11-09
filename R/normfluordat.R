#' Title: Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#'
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#'
#' @author Tingwei Adeck
#'
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#'
#' @import utils
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#'
#' @export
#'
#' @seealso [normfluodat()]
#'
#' @note This function has less optimized space and time complexities than @seealso [normfluodat()].
#' In real-time the difference in optimization is not noticeable.
#' This function also takes less parameters than the more optimized version of the function.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' #' normalized_fluo_dat <- normfluordat(dat=fpath, tnp = 3, cycles = 40)

normfluordat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL){

  df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)

  cleaned_dat_t <- resample_dat_scale(df,tnp=tnp,cycles=cycles)
  fluor_threshold_check(cleaned_dat_t)

  #normalize
  cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

  #name the columns
  ru = rows_used
  cu = cols_used
  usl = user_specific_labels
  sample_col_names <- dat_col_names_prime(cleaned_dat_t, ru, cu, usl)
  colnames(cleaned_dat_t) <- sample_col_names

  #add unique_id
  cleaned_dat_t <-unique_identifier(cleaned_dat_t)

  return(cleaned_dat_t)
}

