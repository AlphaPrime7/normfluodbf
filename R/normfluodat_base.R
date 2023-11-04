#' Title: The root function that returns a normalized data frame with the Cycle No ready for analysis
#' @description
#' Input a dat file (dat file directory) and required parameters and BOOM the researcher has a normalized data frame ripe and ready for clean analysis
#'
#' @author Tingwei Adeck
#' @param dat directory to the users FLUOstar dat file
#' @param tnp Stands for test,negative,positive (sample types); the number should match the number of sample types in the plate reader even if repeating a sample type
#' @param cycles The number of cycles chosen by the researcher. In the case of this package 40 is the standard but ensure to have the right number of samples
#' @param rows_used A character vector of the rows used, eg n = c('A','B','C')
#' @param cols_used  A numeric vector of the columns used, eg m = c(2,4,6)
#' @param user_specific_labels A character vector with specific sample labels based on the plate setup
#'
#' @import utils
#' @import stats
#' @importFrom data.table transpose
#'
#' @return A normalized data frame with the x-variable (Cycle_No), ready for analysis
#' @export
#' @note This is the MAIN function and stands alone but is dependent on the subordinate functions. If the user understands what they are doing this is all they need.
#' The user should use the user_specific_labels parameter for naming variables if they have an extreme unorthodox experimental setup.
#'
#' @seealso [normfluodat()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' n <- c('A','B','C')
#' #' normalized_fluo_dat <- normfluodat_base(dat=fpath, tnp = 3, cycles = 40, n)

normfluodat_base <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL){

  df <- read.table(dat)
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

