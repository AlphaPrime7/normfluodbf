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
#'
#' @import utils
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute. The “Cycle_Number” attribute is the X-variable.
#'
#' @export
#'
#' @note This function is a single-step function leveraging several subordinate functions.
#' It is assumed that the user has the 3 baseline parameters to get this function working.
#' Users must double-check attribute names to ensure they end up with accurate results.
#'
#' @seealso [normfluodatlite()]
#'
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40)

normfluodat <- function(dat, tnp, cycles, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL, norm_scale = NULL,
                        interval= NULL, first_end = NULL, pause_duration=NULL, end_time=NULL){

  df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)

  dat = dat
  ru = rows_used
  usl = user_specific_labels

  #time attribute for OCD people
  interval = interval
  fe = first_end
  pd = pause_duration
  et = end_time
  cycles = cycles

  #Function revamp
  if(is.null(dat) && is.null(tnp) && is.null(cycles)){
    warning("please enter the DAT file path or string, tnp(# of rows) & cycles")

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))
            
          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')

          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))

        if(ncol(df) == 1){
          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')

          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')

          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{

        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{

        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

        df <- as.data.frame(df)
        df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
        df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))

        if(ncol(df) == 1){

          colnames(df) <- sample_col_names
          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))
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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{
        df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
        fluor_threshold_check(df)

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
          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

        } else{

          structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
        fluor_threshold_check(df)

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

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
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }

        } else {

          df <-unique_identifier(df)
          df = df %>% dplyr::relocate('Cycle_Number')
          if(!is.null(interval)){
            ta = time_attribute(interval,fe,pd,et,cycles)
            df = cbind(ta,df)
            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          } else{

            structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

          }
        }

    } else{
      df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
      fluor_threshold_check(df)

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
        structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

      } else{

        structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

      }

    }

  } else {
    df <- resample_dat_scale_optimus(df, tnp = tnp, cycles = cycles)
    fluor_threshold_check(df)

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
      structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

    } else{

      structure(return(df),class= c("normfluodbf_df", "normfluodbf", "normfluodbf_plate_bound", class(df)))

    }

  }

}

#' Title: A normalization applier built on lapply.
#'
#' @description
#' Applies a function over a list of attributes.
#'
#'
#' @param df A data frame.
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale.
#'
#' @return A data frame with attribute values obtained from the applied function using lapply.
#'
#' @export
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- norm_applier(test_df,norm_scale = 'one')

norm_applier <- function(df, norm_scale= c('one','hundred','z-score','raw','decimal')){

  df <- as.data.frame(df)
  if('raw' %in% norm_scale){
    df <- as.data.frame(lapply(df[1:ncol(df)], as.numeric))
    return(df)
  } else if ('one' %in% norm_scale){
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))
  } else if ('hundred' %in% norm_scale){
    df <- as.data.frame(lapply(df[1:ncol(df)], min_max_norm_percent))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))
  } else if ('z-score' %in% norm_scale){
    df <- as.data.frame(lapply(df[1:ncol(df)], norm_z))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))
  } else if('decimal' %in% norm_scale){
    df <- as.data.frame(lapply(df[1:ncol(df)], decimal_scaling))
    df <- as.data.frame(lapply(df[1:ncol(df)], roundfluor))
  }

}
