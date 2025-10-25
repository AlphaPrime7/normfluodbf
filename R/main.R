## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#--------------------------------- Normfluodats ------------------------------

#' Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame
#' with samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1)
#' OR “horizontal” if the user intends to have a final data frame with samples.
#' @param na_omit Takes a string "yes" OR "no".
#' @import utils
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#' @export
#' @seealso [normfluodat()]
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluordat(dat=fpath, tnp = 3, cycles = 40)}
#' @rdname normalize_liposome_fluor_dats
normfluordat <- function(dat,
                         tnp,
                         cycles,
                         rows_used = NULL,
                         cols_used= NULL,
                         user_specific_labels = NULL,
                         read_direction = NULL,
                         na_omit = NULL){

  if (is.data.frame(dat)) {
    df <- dat
  }
  else if (is.character(dat) && file.exists(dat)) {
    df <- tryCatch(
      {
        utils::read.table(dat)
      },
      error = function(e) {
        stop("Error reading the file. Please check the file path and format.")
      }
    )
  }
  else if (is.matrix(dat)) {
    df <- as.data.frame(dat)
  }
  else {
    stop("Input 'dat' must be a data frame, a valid file path, or a matrix.")
  }
  #df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)

  usl = user_specific_labels

  if(is.null(read_direction) || read_direction == 'vertical'){

    if(na_omit == 'yes' || is.null(na_omit)){
      cleaned_dat_t <- resample_dat_scale_naretainer(df,tnp=tnp,cycles=cycles)

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      sample_col_names <- dat_col_names_prime(dat,cleaned_dat_t, ru, cu, usl)
      colnames(cleaned_dat_t) <- sample_col_names

      cleaned_dat_t <- cleaned_dat_t[ , colSums(is.na(cleaned_dat_t))== 0]
      fluor_threshold_check(cleaned_dat_t)

      #normalize
      cleaned_dat_t <- as.data.frame(cleaned_dat_t)
      cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

      if(ncol(df) == 1){
        colnames(cleaned_dat_t) <- sample_col_names
        cleaned_dat_t <- unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)

      } else {
        cleaned_dat_t <- unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)
      }

    } else if(na_omit == 'no' || !is.null(na_omit)){
      cleaned_dat_t <- resample_dat_scale_naretainer(df,tnp=tnp,cycles=cycles)
      fluor_threshold_check(cleaned_dat_t)

      #name the columns
      ru = rows_used
      cu = cols_used
      usl = user_specific_labels
      sample_col_names <- dat_col_names_prime(dat,cleaned_dat_t, ru, cu, usl)
      colnames(cleaned_dat_t) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(cleaned_dat_t), envir = parent.frame())

      #normalize
      cleaned_dat_t <- as.data.frame(cleaned_dat_t)
      cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

      if(ncol(df) == 1){
        colnames(cleaned_dat_t) <- sample_col_names
        cleaned_dat_t <- unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)

      } else {
        cleaned_dat_t <- unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)
      }

    }


  } else if(!is.null(read_direction) || read_direction == 'horizontal'){

    if(na_omit == 'yes' || is.null(na_omit)){
      cleaned_dat_t <- resample_dat_scale_alt_bf_na(df,tnp=tnp,cycles=cycles)

      #name the columns
      ru = rows_used
      cu = cols_used
      #usl = user_specific_labels
      #rd = read_direction
      sample_col_names <- dat_col_names_horizontal(dat,cleaned_dat_t, ru, cu)
      #sample_col_names <- dat_col_names_optimus(dat,cleaned_dat_t, ru, cu, usl, rd)
      colnames(cleaned_dat_t) <- sample_col_names

      cleaned_dat_t <- cleaned_dat_t[ , colSums(is.na(cleaned_dat_t))==0]
      fluor_threshold_check(cleaned_dat_t)

      #normalize
      cleaned_dat_t <- as.data.frame(cleaned_dat_t)
      cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

      if(ncol(df) == 1){
        colnames(cleaned_dat_t) <- sample_col_names
        cleaned_dat_t <-unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)

      } else {
        cleaned_dat_t <-unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)
      }

    } else if(na_omit == 'no' || !is.null(na_omit)){
      cleaned_dat_t <- resample_dat_scale_alt_bf_na(df,tnp=tnp,cycles=cycles)
      fluor_threshold_check(cleaned_dat_t)

      #name the columns
      ru = rows_used
      cu = cols_used
      #usl = user_specific_labels
      #rd = read_direction
      sample_col_names <- dat_col_names_horizontal(dat,cleaned_dat_t, ru, cu)
      #sample_col_names <- dat_col_names_optimus(dat,cleaned_dat_t, ru, cu, usl, rd)
      colnames(cleaned_dat_t) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(cleaned_dat_t), envir = parent.frame())

      #normalize
      cleaned_dat_t <- as.data.frame(cleaned_dat_t)
      cleaned_dat_t <- as.data.frame(lapply(cleaned_dat_t[1:ncol(cleaned_dat_t)], min_max_norm))

      if(ncol(df) == 1){
        colnames(cleaned_dat_t) <- sample_col_names
        cleaned_dat_t <-unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)

      } else {
        cleaned_dat_t <-unique_identifier(cleaned_dat_t)
        cleaned_dat_t = cleaned_dat_t %>% dplyr::relocate('Cycle_Number')
        return(cleaned_dat_t)
      }
    }

  }

}

#' Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file. Also this can be the dirty data frame derived from reading in the dat file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame with
#' samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1) OR “horizontal”
#' if the user intends to have a final data frame with samples arranged as clusters per sample type (A1, A2, A3, B1, B2, B3).
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale; Initialized as NULL.
#' @param interval The time interval chosen for the assay often in seconds.
#' @param first_end The end time of the initial run, often the pause for the introduction of a new substance. This can be the cycle number chosen for the initial stop.
#' @param pause_duration The time between the first end (pause) and resumption of the assay.
#' @param end_time The final end time of the assay.
#' @param normfluodbf.verbose verbose option
#' @import utils
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#' @export
#' @note This function is a single-step function leveraging several subordinate functions.
#' It is assumed that the user has the 3 baseline parameters to get this function working.
#' Users must double-check attribute names to ensure they end up with accurate results.
#' @seealso [normfluodatlite()]
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40)}
#' @rdname normalize_liposome_fluor_dats
normfluodat <- function(dat, tnp = NULL , cycles = NULL, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL,
                        interval= NULL, first_end = NULL, pause_duration=NULL, end_time=NULL, normfluodbf.verbose = TRUE){

  if (is.data.frame(dat)) {
    df <- dat
  }
  else if (is.character(dat) && file.exists(dat)) {
    df <- tryCatch(
      {
        utils::read.table(dat)
      },
      error = function(e) {
        stop("Error reading the file. Please check the file path and format.")
      }
    )
  }
  else if (is.matrix(dat)) {
    df <- as.data.frame(dat)
  }
  else {
    stop("Input 'dat' must be a data frame, a valid file path, or a matrix.")
  }

  if (is.null(tnp)) {
    tnp <- get_tnp(dat)
  }
  else {
    tnp <- tnp
  }

  if (is.null(cycles)) {
    cycles <- actual_cycles(dat)
  }
  else {
    cycles <- cycles
  }

  if (is.null(rows_used)) {
    rows_used <- actual_rows_used(dat)
  }
  else {
    rows_used <- rows_used
  }

  if (is.null(cols_used)) {
    cols_used <- actual_cols_used(dat)
  }
  else {
    cols_used <- cols_used
  }

  #df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)
  ru = rows_used
  usl = user_specific_labels #the user can give the rowcol combination for the well

  #print(rows_used)
  #print(cols_used)
  #print(cycles)
  #print(tnp)

  #time attribute for OCD people
  interval = interval
  fe = first_end
  pd = pause_duration
  et = end_time
  cycles = cycles

  #Function revamp
  if(is.null(dat)){
    warning("please enter the DAT file path or string")

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'raw'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      #ran into mvp issues here i suppose
      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      if(ncol(df) == 1){
        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        #df <- df %>% dplyr::select('Cycle_Number', everything())
        df = df %>% dplyr::relocate('Cycle_Number')

        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')

        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      if(ncol(df) == 1){
        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')

        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')

        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }
    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)

      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')

      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }
    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      df <-unique_identifier(df)

      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }
    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')

      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }

    }

  } else if (!is.null(dat) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }
    }

  } else if( !is.null(dat) && !is.null(tnp) && !is.null(cycles) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        #if(!is.null(interval) && !is.null(first_end) && !is.null(pause_duration) && !is.null(end_time))
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)

      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat, df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      n <- 'na_dataframe'
      assign( paste0(n, '_rawh'), as.data.frame(df), envir = parent.frame())

      df <- df[ , colSums(is.na(df))==0]
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        if(!is.null(interval)){
          ta = time_attribute(interval,fe,pd,et,cycles)
          df = cbind(ta,df)
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        } else{
          class(df) <- c('normfluodbf_normalized_dat', class(df))
          df
        }
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), suppress_messages = TRUE) else fluor_threshold_check(df)

      n <- 'na_dataframe'
      assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      if(!is.null(interval)){
        ta = time_attribute(interval,fe,pd,et,cycles)
        df = cbind(ta,df)
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      } else{
        class(df) <- c('normfluodbf_normalized_dat', class(df))
        df
      }

    }

  } else {
    df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
    if(isFALSE(normfluodbf.verbose)) quiet(fluor_threshold_check(df), all = TRUE)

    n <- 'na_dataframe'
    assign( paste0(n, '_rawv'), as.data.frame(df), envir = parent.frame())

    df <- as.data.frame(df)
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))
    df = df %>% dplyr::relocate('Cycle_Number')
    if(!is.null(interval)){
      ta = time_attribute(interval,fe,pd,et,cycles)
      df = cbind(ta,df)
      class(df) <- c('normfluodbf_normalized_dat', class(df))
      df
    } else{
      class(df) <- c('normfluodbf_normalized_dat', class(df))
      df
    }

  }

}

#' Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame with
#' samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1) OR “horizontal”
#' if the user intends to have a final data frame with samples arranged as clusters per sample type (A1, A2, A3, B1, B2, B3).
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale; Initialized as NULL.
#' @import utils
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#' @export
#' @note This function is a single-step function leveraging several subordinate functions.
#' It is assumed that the user has the 3 baseline parameters to get this function working.
#' Users must double-check attribute names to ensure they end up with accurate results.
#' @seealso [normfluodat()]
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluodatlite(dat=fpath, tnp = 3, cycles = 40)}
#' @rdname normalize_liposome_fluor_dats
normfluodatlite <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL){

  if (is.data.frame(dat)) {
    df <- dat
  }
  else if (is.character(dat) && file.exists(dat)) {
    df <- tryCatch(
      {
        utils::read.table(dat)
      },
      error = function(e) {
        stop("Error reading the file. Please check the file path and format.")
      }
    )
  }
  else if (is.matrix(dat)) {
    df <- as.data.frame(dat)
  }
  else {
    stop("Input 'dat' must be a data frame, a valid file path, or a matrix.")
  }

  #df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)

  ru = rows_used
  usl = user_specific_labels

  #Function revamp
  if(is.null(dat) && is.null(tnp) && is.null(cycles)){
    warning("please enter the DAT file path or string, tnp(# of rows) & cycles")

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'raw'){
    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }


    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }


    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)


    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names


      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)
    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }


    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if (!is.null(dat) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name the columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if( !is.null(dat) && !is.null(tnp) && !is.null(cycles) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      df <- resample_dat_scale_optimus_backend(df, tnp = tnp, cycles = cycles)

      #name columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      df <- resample_dat_scale_alt(df, tnp = tnp, cycles = cycles)

      #name columns
      cu = cols_used
      rd = read_direction
      sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
      colnames(df) <- sample_col_names

      df <- df[ , colSums(is.na(df))==0]
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      if(ncol(df) == 1){

        colnames(df) <- sample_col_names
        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

      } else {

        df <-unique_identifier(df)
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)
      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else {
    df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
    fluor_threshold_check(df)

    df <- as.data.frame(df)
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))
    df = df %>% dplyr::relocate('Cycle_Number')
    return(df)

  }

}

#' Cleans and normalizes DAT files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#' @description
#' The simplest case scenario entails inputting the name or directory of a DAT file as a string,
#' the number of rows denoted by the tnp (test, negative, positive) parameter,
#' and the number of cycles (selected by the user when running the FLUOstar instrument).
#' The program takes these three baseline parameters, performs cleaning and normalization of the DAT file,
#' and then appends an attribute called “Cycle_Number” to the normalized data frame.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param rows_used A character vector of the rows used; ru = c('A','B','C').
#' @param cols_used A numeric vector of the columns used; cu = c(1,2,3).
#' @param user_specific_labels A character vector manually prepared by the user to denote the wells used on the microplate reader; usl = c('A1','B1','C1').
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame with
#' samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1) OR “horizontal”
#' if the user intends to have a final data frame with samples arranged as clusters per sample type (A1, A2, A3, B1, B2, B3).
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale; Initialized as NULL.
#' @param na_omit Takes a string "yes" OR "no".
#' @import utils
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#' @export
#' @note This function is a single-step function leveraging several subordinate functions.
#' It is assumed that the user has the 3 baseline parameters to get this function working.
#' Users must double-check attribute names to ensure they end up with accurate results.
#' @seealso [normfluodat()]
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluodatfull(dat=fpath, tnp = 3, cycles = 40)}
#' @rdname normalize_liposome_fluor_dats
normfluodatfull <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL, na_omit = NULL){

  if (is.data.frame(dat)) {
    df <- dat
  }
  else if (is.character(dat) && file.exists(dat)) {
    df <- tryCatch(
      {
        utils::read.table(dat)
      },
      error = function(e) {
        stop("Error reading the file. Please check the file path and format.")
      }
    )
  }
  else if (is.matrix(dat)) {
    df <- as.data.frame(dat)
  }
  else {
    stop("Input 'dat' must be a data frame, a valid file path, or a matrix.")
  }

  #df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)
  actual_cols_u <- actual_cols_used(dat)

  #define naming parameters globally
  ru = rows_used
  usl = user_specific_labels

  #Function revamp
  if(is.null(dat) && is.null(tnp) && is.null(cycles)){
    warning("please enter the DAT file path or string, tnp(# of rows) & cycles")

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'raw'){
    if(is.null(read_direction) || read_direction == 'vertical'){

      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        #apply procedure
        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names
        #remove NA cols to ensure the best approximation of sample names
        #df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        #df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }
      }
    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(read_direction) || read_direction == 'vertical'){

      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)


    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){

      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }
  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'z-score'){
    if(is.null(read_direction) || read_direction == 'vertical'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{

      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if(!is.null(dat) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(read_direction) || read_direction == 'vertical'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }
      else {
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        #add unique_id
        df <-unique_identifier(df)
        colnames(df) <- c(1:(ncol(df)-1))
        df = df %>% dplyr::relocate('Cycle_Number')
        return(df)

        return(df)

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if (!is.null(dat) && !is.null(tnp) && !is.null(cycles) && !is.null(rows_used) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

        #name the columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')
      return(df)

    }

  } else if( !is.null(dat) && !is.null(tnp) && !is.null(cycles) ){

    if(is.null(read_direction) || read_direction == 'vertical'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg

        #name columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_optimus_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg

        #name columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else if(!is.null(read_direction) || read_direction == 'horizontal'){
      if(is.null(na_omit) || na_omit == 'yes') {
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg

        #name columns
        cu = cols_used
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        #remove NA cols to ensure the best approximation of sample names
        df <- df[ , colSums(is.na(df))==0]

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }


      } else if(!is.null(na_omit) || na_omit == 'no'){
        df <- resample_dat_scale_alt_na(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df) #needs to be changed in pkg

        #name columns
        ru = rows_used
        cu = cols_used
        usl = user_specific_labels
        rd = read_direction
        sample_col_names <- dat_col_names_optimus(dat,df, ru, cu, usl, rd)
        colnames(df) <- sample_col_names

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          return(df)
        }

      }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles) #needs to change in package
      fluor_threshold_check(df) #needs to be changed in pkg

      df <- as.data.frame(df)
      df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
      df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

      #add unique_id
      df <-unique_identifier(df)
      colnames(df) <- c(1:(ncol(df)-1))
      df = df %>% dplyr::relocate('Cycle_Number')

      return(df)

    }

  } else {
    df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles) #needs to change in package
    fluor_threshold_check(df) #needs to be changed in pkg

    df <- as.data.frame(df)
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

    #add unique_id
    df <-unique_identifier(df)
    colnames(df) <- c(1:(ncol(df)-1))
    df = df %>% dplyr::relocate('Cycle_Number')

    return(df)
  }
}

# ---------------------------- Normfluodbfs -----------------------------------

#' Cleans and Normalizes DBF files obtained from experiments using the FLUOstar Omega microplate reader (from BMG LABTECH).
#' @family normfluodbf
#' @description
#' The simplest function utilization scenario entails an input of the path to a DBF file obtained from the FLUOstar microplate (usually a 96-well microplate) reader;
#' In a single step, this function will create a data frame, clean the data frame, normalize the data frame, append a "Cycle_Number" attribute,
#' perform an adjustment to the “time” attribute and return a data frame that is ready for analysis.
#' Since the initial publication of this package, several changes have been made to improve the user experience and to give the user more options
#' to fine-tune the output from the package to meet the users’ aesthetic needs.
#' Users who decide to move past the simplest utility scenario have been given more options to customize the output based on the users’ needs.
#' Notably, several normalization sub-parameters have been provided in the package which yields different outputs based on what the user is used to seeing.
#' Just as the FLUOstar instrument is built to handle an array of assays,
#' this function is designed to be multi-dimensional (meaning it can handle data with the same DBF extension from other assay types),
#' on the condition that the data from assay types other than liposome flux assays follow the same data format this package was designed to handle.
#' Of course, users of this package are advised to pre-analyze DBF files from other assay types to ensure they are compliant with this package (compliance in this scenario is simple meaning DBF files from other assays should be like DBF files from liposome flux assays).
#' @author Tingwei Adeck
#' @param file A string ("liposomes_xxx.dbf") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dbf" file.
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ , which denotes the normalization type or scale; The parameter is initialized as NULL.
#' @param transformed This parameter takes input 'log', which denotes a logarithmic box-cox transformation; Initialized as NULL.
#' @param fun A parameter defined as NA is used for Boolean expressions or manipulation.
#' @param ... An abstract placeholder or container parameter that can be used to capture extra variables if needed.
#' @importFrom data.table transpose
#' @import foreign
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#' @seealso [normfluordbf()], [normfluodat()]
#' @note
#' The default normalization sub-parameter outputs values in the 0-1 range.
#' Unless a “norm_scale” level is specified by the user, the default output is in the 0-1 range.
#' The “norm_scale” sub-parameter “decimal” is a machine-learning tool and should be avoided;
#' it also provides no advantage for basic research analysis as its output operates on a sliding scale just like the raw data.
#' Logarithmic transformation provides a minuscule advantage in data analysis and could/should be avoided.
#' Backward compatibility is maintained in all updates, so there should be no issues with using the package the way the user was used to.
#' The favorite "norm_scale" level is "z-score" since it divides the axis into negative and positive, thus facilitating interpretation.
#' @examples \dontrun{
#' fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' normalized_dbf <- norm_tidy_dbf(file=fpath, norm_scale = 'raw')
#' normalized_dbf <- normfluordbf(file=fpath, norm_scale = 'raw')}
#' @name liposome_fluor_dbfs
NULL

#' @rdname liposome_fluor_dbfs
#' @family normfluodbf
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#' @keywords internal
.normfluordbf <- function(x, fun = NA, ...){
  y <- transpose(l=x)
  rownames(y) <- colnames(x)
  colnames(y) <- rownames(x)
  colnames(y) <- paste0("a",rownames(x))

  sample_col_names<- vector("list")
  nofun <- is.na(fun)
  for(j in y[1,]){
    if(is.na(j) != nofun){
      sample_col_names <- c(sample_col_names,j)
    }
  }

  nofun <- is.na(fun)
  dirty_time <- y[,1]
  dbf_time_column <- data.frame() #can be a matrix wrapped into a df
  for(i in dirty_time){
    if(is.na(i) != nofun && i != "t"){
      dbf_time_column <- rbind(dbf_time_column,i)
    }
  }
  colnames(dbf_time_column) <- c('Time')

  y[1:3,] <- NA
  y <- y %>% drop_na()
  y <- y[,-(1:2)]
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
  colnames(y) <- sample_col_names
  y <- cbind(y,dbf_time_column)
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  y["Time"] = y[,"Time"] + 30
  return(unique_identifier(y))

}

#' @rdname liposome_fluor_dbfs
#' @family normfluodbf
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#' @export
norm_tidy_dbf <- function(file = NULL, norm_scale = NULL, transformed = NULL, fun = NA, ...){
  x <- foreign::read.dbf(file=file, as.is = F)
  y <- data.table::transpose(l=x)
  rownames(y) <- colnames(x)
  colnames(y) <- rownames(x)
  colnames(y) <- paste0("a",rownames(x))

  sample_col_names<- vector("list")
  nofun <- is.na(fun)
  for(j in y[1,]){
    if(is.na(j) != nofun){
      sample_col_names <- c(sample_col_names,j)
    }
  }

  nofun <- is.na(fun)
  dirty_time <- y[,1]
  dbf_time_column <- data.frame()
  for(i in dirty_time){
    if(is.na(i) != nofun && i != "t"){
      dbf_time_column <- rbind(dbf_time_column,i)
    }
  }
  colnames(dbf_time_column) <- c('Time')

  y[1:3,] <- NA
  y <- y %>% tidyr::drop_na()
  y <- y[,-(1:2)]
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  n <- 'liposome_dataframe'
  assign( paste0(n, '_raw'), as.data.frame(y), envir = parent.frame())
  fluor_threshold_check(y)

  if(is.null(file)){
    warning("please enter a string for the .dbf file you want to normalize")

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'raw'){

    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if (!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'raw' && transformed == 'log'){

    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'one' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'hundred' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'z-score' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'decimal' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)

  } else if (!is.null(file)){

    if(is.null(norm_scale) && !is.null(transformed) && transformed == 'log'){
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30

      y = unique_identifier(y)
      #y = y %>% dplyr::relocate('Cycle_Number', 'Time')
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30

      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    }
  }
}

#' @rdname liposome_fluor_dbfs
#' @family normfluodbf
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#' @export
normfluordbf <- function(file = NULL, norm_scale = NULL, transformed = NULL, fun = NA, ...){
  x <- foreign::read.dbf(file=file, as.is = F)
  #x <- rio::import(file)
  y <- data.table::transpose(l=x)
  rownames(y) <- colnames(x)
  colnames(y) <- rownames(x)
  colnames(y) <- paste0("a",rownames(x))

  sample_col_names <- vector("list")
  nofun <- is.na(fun)
  for(j in y[1,]){
    if(is.na(j) != nofun){
      sample_col_names <- c(sample_col_names,j)
    }
  }

  nofun <- is.na(fun)
  dirty_time <- y[,1]
  dbf_time_column <- data.frame()
  for(i in dirty_time){
    if(is.na(i) != nofun && i != "t"){
      dbf_time_column <- rbind(dbf_time_column,i)
    }
  }
  colnames(dbf_time_column) <- c('Time')

  y[1:3,] <- NA
  y <- y %>% tidyr::drop_na()
  y <- y[,-(1:2)]
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  colnames(y) <- sample_col_names
  n <- 'liposome_dataframe'
  assign( paste0(n, '_raw'), as.data.frame(y), envir = parent.frame())
  fluor_threshold_check(y)

  if(is.null(file)){
    warning("please enter a string for the .dbf file you want to normalize")

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'raw'){

    if(is.null(transformed)){
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    } else{
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    }

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    }

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    }
  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    }
  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)

    } else{
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      class(y) <- c('normfluodbf_normalized_dbf', class(y))
      return(y)
    }

  } else if (!is.null(file)){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30
    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    class(y) <- c('normfluodbf_normalized_dbf', class(y))
    return(y)
  }
}

#' @rdname liposome_fluor_dbfs
#' @family normfluodbf
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#' @note The norm_scale must be provided if the user chooses to use this option.
#' @export
#' @examples
#' \dontrun{wells = normfluodbf(lipsum_214, norm_scale = 'hundred')}
normfluodbf <- function(file = NULL, norm_scale, transformed = NULL, fun = NA, ...) {

  if(is.null(norm_scale)){
    rlang::abort(
      message = sprintf('The %s parameter must be provided when using normfluodbf','norm_scale'),
      use_cli_format = TRUE
    )
  }
  x <- foreign::read.dbf(file = file, as.is = F)
  y <- data.table::transpose(l = x)
  rownames(y) <- colnames(x)
  colnames(y) <- paste0("a", rownames(x))

  sample_col_names <- y[1, ]
  sample_col_names <- sample_col_names[!is.na(sample_col_names) == is.na(fun)]

  dirty_time <- y[, 1]
  dbf_time_column <- data.frame(Time = dirty_time[!is.na(dirty_time) == is.na(fun) & dirty_time != "t"])

  y[1:3, ] <- NA
  y <- tidyr::drop_na(y)
  y <- y[, -(1:2)]
  y[] <- lapply(y, as.numeric)
  colnames(y) <- sample_col_names

  assign('liposome_dataframe_raw', as.data.frame(y), envir = parent.frame())
  fluor_threshold_check(y)

  if (is.null(file)) {
    warning("please enter a string for the .dbf file you want to normalize")
    return()
  }

  normalize_data <- function(data, transformed, norm_func, dbf_time_column) {
    if (!is.null(transformed)) {
      data <- lapply(data, log_transformation)
    }
    data <- as.data.frame(lapply(data, norm_func))
    data <- as.data.frame(lapply(data, roundfluor))
    colnames(data) <- sample_col_names
    data <- cbind(data, dbf_time_column)
    data[] <- lapply(data, as.numeric)
    data["Time"] <- data["Time"] + 30
    data <- unique_identifier(data)
    data <- data %>% dplyr::relocate('Time', 'Cycle_Number')
    class(data) <- c('normfluodbf_normalized_dbf', class(data))
    return(data)
  }

  norm_func <- switch(norm_scale,
                      "one" = min_max_norm,
                      "hundred" = min_max_norm_percent,
                      "z-score" = norm_z,
                      "decimal" = decimal_scaling,
                      min_max_norm)

  return(normalize_data(y, transformed, norm_func, dbf_time_column))
}
