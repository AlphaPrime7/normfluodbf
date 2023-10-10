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
#'
#' @import utils
#' @import stats
#'
#' @return A normalized data frame with the x-variable (Cycle_No), ready for analysis
#' @export
#' @note This is the MAIN function and stands alone but is dependent on the subordinate functions. If the user understands what they are doing this is all they need.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' n <- c('A','B','C')
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, n)

normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL){

  df <- utils::read.table(dat) #dat becomes df
  df <- clean_odddat(df)
  df <- comma_cleaner(df)
  df <- as.data.frame(df)

  col_list <- c()
  for(i in 1:ncol(df)){
    n <- "a"
    col_list <- c(col_list,assign(paste0(n, i), as.data.frame(df[,i])) )
  }

  j_vect <- c()
  for(j in col_list){
    j <- as.data.frame(j)
    j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
    j_dfs <- as.data.frame(j_resampled)
    j_vect <- c(j_vect, j_dfs)
  }

  cleaned_dat = do.call(rbind, j_vect)
  cleaned_dat = as.data.frame(cleaned_dat)
  cleaned_dat_t = data.table::transpose(l=cleaned_dat)

  #normalize
  cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

  #name the columns
  ru = rows_used
  cu = cols_used
  sample_col_names <- dat_col_names(cleaned_dat_t, ru, cu)
  colnames(cleaned_dat_t) <- sample_col_names

  #add unique_id
  cleaned_dat_t <-unique_identifier(cleaned_dat_t)

  suppressWarnings(return(cleaned_dat_t))
}


