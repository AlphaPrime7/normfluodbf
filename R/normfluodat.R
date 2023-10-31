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
#' @param read_direction User can leave null for machine up-down read OR 'horizontal' for machine left-right read
#' @param norm_scale The normalization scale
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
#' @seealso [normfluordbf()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' n <- c('A','B','C')
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, n, read_direction = 'vertical')
#' #' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40)

normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL){

  df <- utils::read.table(dat)
  #df <- clean_odd_cc(df)
  df <- clean_odddat_optimus(df)
  fluor_threshold_check_na(df)
  fluor_threshold_check_raw(df)

  #Function revamp
  if(is.null(dat) && is.null(tnp) && is.null(cycles)){
    warning("please enter the DAT file path or string, tnp(# of rows) & cycles")

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'raw'){
    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }

  } else if (!is.null(dat) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      return(df)

    }

  } else if( !is.null(dat) && !is.null(tnp) && !is.null(cycles) ){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df) #needs to be changed in pkg
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)
      #colnames(df) <- c(1:(ncol(df)-1))

      return(df)

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df) #needs to be changed in pkg
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)
      #colnames(df) <- c(1:(ncol(df)-1))

      return(df)

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles) #needs to change in package
      fluor_threshold_check(df) #needs to be changed in pkg
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #name columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      #add unique_id
      df <-unique_identifier(df)
      #colnames(df) <- c(1:(ncol(df)-1))

      return(df)

    }

  }

}


