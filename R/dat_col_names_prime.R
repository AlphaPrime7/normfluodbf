#' Title: Attribute(s) naming function.
#'
#' @description
#' This function is used to name attribute(s).
#' Attribute(s) names, in this case, are equivalent to the well labels found on the microplate reader.
#' An attribute for a sample loaded into row A - column 1 will be named A1.
#' In short, the function takes a clean data frame and returns attribute names
#' that match the FLUOstar plate layout often presented as an Excel file.
#'
#'
#' @author Tingwei Adeck
#'
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param df A data frame that requires attribute labels.
#' @param rows_used A character vector indicating the rows or tuples used on the microplate (usually a 96-well microplate). Initialized as NULL.
#' @param cols_used A numeric vector indicating the plate columns or attributes used. Initialized as NULL.
#' @param user_specific_labels A character vector where the user manually enters the used microplate wells based on the FLUOstar plate layout.
#'
#' @return Returns a character vector of attribute(s) names for the normalized data frame.
#'
#' @export
#'
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' n = c('A','B','C')
#' sample_col_names <- dat_col_names_prime(dat = fpath, resampled_scaled, n)

dat_col_names_prime <- function(dat = NULL, df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL){

  actual_cols <- actual_cols_used(dat)
  if(is.null(cols_used)){
    cols_used = actual_cols
  }

  colnames_noru <- c(1:ncol(df))

  if(is.null(rows_used)){
    message('The User is advised to input a character vector of rows used')
    return(colnames_noru)
  }

  col_names <- c()
  if(!is.null(cols_used)){
    normal_sequence = c(min(cols_used):max(cols_used))
  } else {
    normal_sequence = NULL
  }

  if(!is.null(user_specific_labels)){
    return(user_specific_labels)

  } else if(is.null(cols_used)){
    cols_used = actual_cols
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    return(col_names[1:ncol(df)])

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    if(sum(cols_used) != sum(actual_cols)){
      cols_used = actual_cols
    } else {
      cols_used = cols_used
    }
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    return(col_names[1:ncol(df)])

  } else if(is.null(user_specific_labels) && ncol(df) < length(cols_used)*length(rows_used) || ncol(df) > length(cols_used)*length(rows_used) || length(cols_used) < length(normal_sequence) ){

    if(sum(cols_used) != sum(actual_cols)){
      cols_used = actual_cols
    } else {
      cols_used = cols_used
    }
    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(user_specific_labels)){
      print(col_names[1:(length(cols_used)*length(rows_used))])
      message('From the list printed above, enter the columns used based on your microplate layout;')
      choose_cols_used=scan(what=character(), n=ncol(df))
      print(choose_cols_used)
      return(as.vector(choose_cols_used))
    } else{
      return(user_specific_labels)
    }
  }
}
