## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' A function to append a unique identifier attribute to any data frame within the normfluodbf package.
#' @description
#' The function in the context of normfluodbf creates an attribute called Cycle_Number
#' and appends this attribute to the cleaned or wrangled data frame derived from the dirty DBF file.
#' @author Tingwei Adeck
#' @param df A data frame with 1:n number of rows.
#' @return A data frame with the Cycle_Number attribute appended to the end of the data frame.
#' @export
#' @seealso [normfluodat()]
#' @note The function operates in a closed system,
#' meaning it is primarily designed to work with this package ONLY.
#' Other use cases are simply a coincidence.
#' @examples \dontrun{
#' test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' unique_identifier(test_df)}
#' @rdname normfluodbf_utils
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$Cycle_Number[i] <- x
  }
  return(df)
}

#' A function to append a generic identifier attribute to any data frame, but users supply a name for said attribute.
#' @description
#'A function that creates an attribute of seq(numrows) with a step size of 1, where the user provides the attribute name.
#' @author Tingwei Adeck
#' @param numrows The number of rows the user intends to have in the created data frame.
#' @param col_name The desired attribute name.
#' @return A user-named single attribute data frame with nrow = numrows.
#' @export
#' @examples \dontrun{
#' generic_identifier(40, col_name="Cycle_No")}
#' @rdname normfluodbf_utils
generic_identifier <- function(numrows, col_name){
  vect <- seq(numrows)
  df <- as.data.frame(vect)
  colnames(df) <- c(col_name)
  return(df)
}

#' Normalizing Agents
#' @family normfluodbf_utils
#' @param x value(s)
#' @param df data frame
#' @return A normalized value when applied to a single value or a normalized attribute with values between the normalizing range.
#' @examples
#' \dontrun{
#' test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm)}
#' @name normalizingagents
NULL

#' @rdname normalizingagents
#' @return normalized value (0-1)
#' @export
min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#' @rdname normalizingagents
#' @return normalized value (0-1)
#' @export
min_max_norm_df <- function(df) {
  return(as.data.frame(lapply(df, function(x) (x - min(x)) / (max(x) - min(x)))))
}

#' @rdname normalizingagents
#' @return normalized value (0-100)
#' @export
min_max_norm_percent <- function(x){
  ((x - min(x)) / (max(x) - min(x))) * 100
}

#' @rdname normalizingagents
#' @return normalized value (0-100)
#' @export
min_max_norm_percent_df <- function(df) {
  return(as.data.frame(lapply(df, function(x) ((x - min(x)) / (max(x) - min(x))) * 100)))
}

#' @rdname normalizingagents
#' @return normalized value (Z = N (0,1))
#' @export
norm_z <- function(x){
  (x - mean(x)) / stats::sd(x)
}

#' @rdname normalizingagents
#' @return normalized value (Z = N (0,1))
#' @export
norm_z_df <- function(df) {
  return(as.data.frame(lapply(df, function(x) (x - mean(x)) / stats::sd(x))))
}

#' @rdname normalizingagents
#' @return normalized value
#' @export
decimal_scaling <- function(x){
  max_abs <- max(abs(x))
  power <- ceiling(log10(max_abs))
  x/(10^power)
}

#' @rdname normalizingagents
#' @return normalized value
#' @export
decimal_scaling_df <- function(df) {
  return(as.data.frame(lapply(df, function(x) x / 10^ceiling(log10(max(abs(x)))))))
}

#' @rdname normalizingagents
#' @return log value
#' @export
log_transformation <- function(x){
  log(x)
}

#' @rdname normalizingagents
#' @return rounded value
#' @export
roundfluor <- function(x){
  round(x, 5)
}

#' A normalization applier built on lapply.
#' @description
#' Applies a function over a list of attributes.
#' @param df A data frame.
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ ,
#' which denotes the normalization type or scale.
#' @return A data frame with attribute values obtained from the applied function using lapply.
#' @export
#' @examples \dontrun{
#' test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- norm_applier(test_df,norm_scale = 'one')}
#' @rdname normfluodbf_utils
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

#' A time attribute generating function
#' @description
#' A function to help the researcher reproduce the time attribute.
#' @param interval The time interval chosen for the assay often in seconds.
#' @param first_end The end time of the initial run, often the pause for the introduction of a new substance. This can be the cycle number chosen for the initial stop.
#' @param pause_duration The time between the first end (pause) and resumption of the assay.
#' @param end_time The final end time of the assay.
#' @param cycles The number of cycles in the assay as selected by the user or researcher.
#' @return The time attribute.
#' @keywords internal
#' @note
#' The original function had an option for minutes which was for less time conscious people
#' but the final version for this package has no such option. Users MUST provide numbers in
#' seconds.
#' @examples \dontrun{
#' time_test = time_attribute(30,8,136,1276,40)
#' time_test = time_attribute(60,8,136,2460,40)}
#' @rdname normfluodbf_utils
time_attribute = function(interval= NULL, first_end = NULL, pause_duration=NULL, end_time=NULL, cycles=NULL){

  start_time = 0

  if(is.null(interval)){
    warning('Enter the cycle interval in seconds as setup in the machine')
  }

  if(pause_duration < interval || is.null(pause_duration)){
    pause_duration = interval
  } else{
    pause_duration = pause_duration
  }

  # if('cycles' %in% time_unit || is.null(time_unit) && !is.null(first_end))

  if(!is.null(first_end) && !is.null(end_time) && !is.null(pause_duration)){

    first_end = (first_end-1) * interval

    #before_pause
    first_end = seq(from=start_time,to=first_end,by=interval)

    #new sequence start
    timer_resume = tail(first_end,1)  + pause_duration

    #after_pause
    after_pause = seq(from=timer_resume,to=end_time,by=interval)

    #final time attribute
    assay_time = append(first_end,after_pause)
    assay_time = assay_time[1:cycles]
    assay_time = as.data.frame(assay_time)
    colnames(assay_time) = c('Time')
    return(assay_time)
  } else {
    assay_time = seq(from=start_time,  by = interval, along.with=seq(cycles))
    assay_time = as.data.frame(assay_time)
    colnames(assay_time) = c('Time')

    return(assay_time)
  }
}

#' A function to get the actual rows used in the assay.
#' @description
#' A function that facilitates a users' workflow by helping extract the actual rows used in the assay.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @return Returns a character vector denoting the rows used in the assay.
#' @export
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' arutest <- actual_rows_used(fpath)}
#' @rdname normfluodbf_utils
actual_rows_used <- function(dat){
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

  H_position <- which(LETTERS == "H")
  length_A_to_H <- H_position

  df <- head(df, length_A_to_H)
  rows_without_all_dashes <- which(!apply(df, 1, function(row) all(row %in% c('-,', '-'))))
  row_letters <- LETTERS[rows_without_all_dashes]
  return(row_letters)
}

#' A function to get the number of rows used.
#' @description
#' A function that facilitates a users' workflow by helping to get the number of rows used in the assay.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @return Returns the number of rows used denoted as tnp.
#' @export
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' gettnptest <- get_tnp(fpath)}
#' @rdname normfluodbf_utils
get_tnp <- function(dat){
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

  H_position <- which(LETTERS == "H")
  length_A_to_H <- H_position

  df <- head(df, length_A_to_H)
  rows_without_all_dashes <- which(!apply(df, 1, function(row) all(row %in% c('-,', '-'))))
  row_letters <- LETTERS[rows_without_all_dashes]
  return(length(row_letters))
}

#' A function to get the cycles.
#' @description
#' A function to get the number of cycles used in the assay.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @return The number of cycles.
#' @export
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' getcyclestest <- actual_cycles(fpath)}
#' @rdname normfluodbf_utils
actual_cycles <- function(dat){
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

  H_position <- which(LETTERS == "H")
  length_A_to_H <- H_position
  number_of_cycles <- nrow(df) / length_A_to_H

  return(number_of_cycles)
}

#' A function to get the actual columns used in the assay.
#' @description
#' A function that facilitates a users' workflow by helping extract the actual columns used in the assay.
#' @author Tingwei Adeck
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @return Returns a numeric vector denoting the columns used in the assay.
#' @export
#' @examples \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' acutest <- actual_cols_used(fpath)}
#' @rdname normfluodbf_utils
actual_cols_used <- function(dat){
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

  df <- clean_odddat_optimus(df)
  colnames(df) <- c(1:ncol(df))
  acu <- names(which(colSums(!is.na(df)) > 0))
  acu <- as.numeric(as.vector(acu))
  return(acu)
}


#' A fluorescence quantification Quality Control (QC) function.
#' @family normfluodbf_utils
#' @description
#' A function designed to check that fluorescence values do not exceed the upper limit (2^15 or 32768)
#' OR fall below the lower limit (2^11 or 2048).
#' Fluorescence values that exceed these thresholds are considered noisy
#' and lead to incorrect interpretation of analysis results.
#' @author Tingwei Adeck
#' @param clean_df A cleaned data frame.
#' @param fun A parameter used for Boolean expressions.
#' @import emojifont
#' @return A polite warning message to the researcher.
#' @note
#' Experimental issues should be investigated at very high or very low fluorescence values.
#' The most common experimental issues arise when ACMA concentrations are out of the tolerated range.
#' Based on my experience, ACMA concentrations between 2 and 5 Micromolar will suffice
#' to get fluorescence values within the tolerance threshold.
#' ACMA concentrations as low as 0.2 Micromolar or as high as 20 Micromolar have proven problematic based on my research experience.
#' A second issue linked to the FLUOstar instrument revolves around setting the right “gain”
#' to ensure the right level of sensitivity in machine readings.
#' A very high “gain” setting results in increased machine sensitivity even
#' at the right ACMA concentrations and vice versa. In short,
#' we want the machine to be primed to read exactly what we feed it, no more, no less.
#' This function provides the attribute(s) and tuple(s) for the values that need investigation.
#' These deductions were obtained from my experimental hiccups and my characterization of the liposome flux assay system.
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' resampled_scaled <- resampled_scaled[,c(1:4)]
#' fluor_threshold_check(resampled_scaled)}
#' @name fluorthresholdcheck
NULL

#' @rdname fluorthresholdcheck
#' @return outlier wells
#' @export
fix_threshold_output <- function(outlier_wells){
  well_vector <- unlist(outlier_wells)
  cleaned_well_vector <- well_vector[well_vector != "Time" & well_vector != "Cycle_Number"]
  cleaned_well_list <- as.list(cleaned_well_vector)
  cleaned_well_list
}

#' @rdname fluorthresholdcheck
#' @return outlier wells list
#' @keywords internal
.fluor_threshold_check <- function(clean_df, fun = NA){
  load.emojifont(font = "EmojiOne.ttf")
  nofun <- is.na(fun)
  clean_df <- rbind(clean_df, NA)
  clean_df <- cbind(clean_df, NA)

  outlier_wells <- list()

    for(i in 1:nrow(clean_df)){
      for(j in 1:ncol(clean_df)){
        if ( clean_df[i,j] >= (2^15) && is.na(clean_df[i,j]) != nofun ){
          message(c(emoji('pig'), emoji('camel'), emoji('lion')))
          message(paste("YIKES, value > 2^15, Watch in future experimental designs",'column:', j , 'row:', i))
          outlier_wells <- c(outlier_wells, names(clean_df)[j])
        } else if ( clean_df[i,j] <= (2^11) && is.na(clean_df[i,j]) != nofun ){
          message(c(emoji('pig'), emoji('camel')))
          message(paste("YIKES, value < 2^11, Watch in future experimental designs",'column:', j , 'row:', i))
          outlier_wells <- c(outlier_wells, names(clean_df)[j])
        }
      }
    }
  outlier_wells <- unique(outlier_wells)
  print("Outlier wells (Mixtures might be problematic and should be investigated with mixtools)")
  print(outlier_wells)
  return(fix_threshold_output(outlier_wells))
}

#' @rdname fluorthresholdcheck
#' @return outlier wells list
#' @export
fluor_threshold_check <- function(clean_df, fun = NA){
  load.emojifont(font = "EmojiOne.ttf")
  nofun <- is.na(fun)
  clean_df <- rbind(clean_df, NA)
  clean_df <- cbind(clean_df, NA)

  outlier_wells <- list()

  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if ( clean_df[i,j] >= (2^15) && is.na(clean_df[i,j]) != nofun ){
        outlier_wells <- c(outlier_wells, names(clean_df)[j])
      } else if ( clean_df[i,j] <= (2^11) && is.na(clean_df[i,j]) != nofun ){
        outlier_wells <- c(outlier_wells, names(clean_df)[j])
      }
    }
  }
  outlier_wells <- unique(outlier_wells)
  message(paste("Crikee, some values in your original data violate thresholds", emoji('pig'), emoji('camel')))
  print("Outlier wells (Mixtures might be problematic and should be investigated with mixtools)")
  print(outlier_wells)
  return(fix_threshold_output(outlier_wells))
}

#' @rdname fluorthresholdcheck
#' @return outlier wells
#' @export
fix_threshold_output <- function(outlier_wells){
  well_vector <- unlist(outlier_wells)
  cleaned_well_vector <- well_vector[well_vector != "Time" & well_vector != "Cycle_Number"]
  cleaned_well_list <- as.list(cleaned_well_vector)
  cleaned_well_list
}

#' @rdname fluorthresholdcheck
#' @return warning printout
#' @note designed to check for NA values
#' @export
is_threshold_violated <- function(clean_df, fun = NA){
  load.emojifont(font = "EmojiOne.ttf")
  nofun <- is.na(fun)
  for(i in 1:nrow(clean_df)){
    for(j in 1:ncol(clean_df)){
      if( (clean_df[i,j] >= 2^15 || clean_df[i,j] <= 2^11)  && is.na(clean_df[i,j]) != nofun ){
        warning(paste("Crikee, some values in your original data violate thresholds", emoji('pig'), emoji('camel')))
        return(TRUE)
      } else{
        return(FALSE)
      }
    }
  }
}

#' @rdname fluorthresholdcheck
#' @return warning printout
#' @note designed to check for NA values
#' @export
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
