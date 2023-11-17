#' Title: Attribute(s) naming function.
#'
#' @description
#' This function is used to name attribute(s).
#' Attribute(s) names, in this case, are equivalent to the well labels found on the microplate reader.
#' An attribute for a sample loaded into row A - column 1 will be named A1.
#' In short, the function takes a clean data frame and returns attribute names
#' that match the FLUOstar plate layout often presented as an Excel file.
#'
#' @author Tingwei Adeck
#'
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#' @param df A data frame that requires attribute labels.
#' @param rows_used A character vector indicating the rows or tuples used on the microplate (usually a 96-well microplate). Initialized as NULL.
#' @param cols_used A numeric vector indicating the plate columns or attributes used. Initialized as NULL.
#' @param user_specific_labels A character vector where the user manually enters the used microplate wells based on the FLUOstar plate layout.
#' @param read_direction A string input with two choices, “vertical” or “horizontal.”
#' The user indicates “vertical” if the user intends to have a final data frame
#' with samples arranged as sample type triplets (A1, B1, C1, A1, B1, C1)
#' OR “horizontal” if the user intends to have a final data frame with samples
#' arranged as clusters per sample type (A1, A2, A3, B1, B2, B3).
#'
#' @import stringr
#'
#' @return Returns a character vector of attribute(s) names for the normalized data frame.
#'
#' @export
#'
#' @note Users are advised to input rows used but won’t be penalized for not doing so.
#' If the user provides the rows used, then attribute names are generated for the user.
#' The user must check to ensure that the names match the microplate layout.
#'
#' The user can leave the columns used as NULL if the user loaded samples from column 1 and did so in sequence.
#' If the user fails to load in sequence from the first position, then the user must provide a numeric vector of columns used.
#'
#' For instance, where the user skips columns, the user will be prompted to interact
#' with the program in order to ensure the final data frame has the correct attribute names.
#'
#' The user can bypass the rows used and columns used parameters
#' if the user supplies a manually created character vector of the wells used in an experiment.
#'
#' The read direction parameter is used to determine the presentation of the samples in the final data frame.
#'
#'
#' @seealso [dat_col_names_optimus()]
#'
#' @note This naming function only returns a character vector hence the rigid suffix.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' n = c('A','B','C')
#' sample_col_names <- dat_col_names_rigid(dat = fpath, resampled_scaled, n)

dat_col_names_rigid <- function(dat = NULL, df, rows_used = NULL, cols_used= NULL, user_specific_labels = NULL, read_direction = NULL){

  actual_cols <- actual_cols_used(dat)

  if(is.null(rows_used)){
    warning('The user is advised to input a character vector of rows used')
  }

  col_names <- c()
  col_names_sort <- c()
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
    if(is.null(read_direction) || read_direction == 'vertical'){
      message('Check the data frame for correct attribute names AND attributes without names')
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      message('Check the data frame for correct attribute names AND attributes without names')
      return(col_names_sort[1:ncol(df)])
    }

  } else if(!is.null(rows_used) && !is.null(cols_used) && ncol(df) == length(cols_used)*length(rows_used) && length(cols_used) <= length(normal_sequence) ){

    for(i in cols_used){
      col_names <- c(col_names, paste0(rows_used,i))
    }
    if(is.null(read_direction) || read_direction == 'vertical'){
      return(col_names[1:ncol(df)])
    } else if(read_direction == 'horizontal'){
      col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
      return(col_names_sort[1:ncol(df)])
    }
  } else if(is.null(user_specific_labels) || ncol(df) < length(cols_used)*length(rows_used) && length(cols_used) < length(normal_sequence) ){
    if(ncol(df) > length(cols_used)*length(rows_used)){
      message('The number of columns exceeds the users estimate')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:ncol(df)] )
        message('From the list printed above, enter the columns used based on your microplate layout;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:ncol(df)] )
        message('From the list printed above, enter the columns used based on your microplate layout;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }

    }else {
      message('The number of columns is less than the users estimate')
      for(i in cols_used){
        col_names <- c(col_names, paste0(rows_used,i))
      }
      if(is.null(user_specific_labels) && is.null(read_direction) || read_direction == 'vertical'){
        print(col_names[1:(length(cols_used)*length(rows_used))])
        message('From the list printed above, enter the columns used based on your microplate layout;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
        return(as.vector(choose_cols_used))
      } else if(is.null(user_specific_labels) && !is.null(read_direction) && read_direction == 'horizontal'){
        col_names_sort <- stringr::str_sort(col_names, decreasing = F, na_last = T, locale = 'en', numeric = T)
        print(col_names_sort[1:(length(cols_used)*length(rows_used))])
        message('From the list printed above, enter the columns used based on your microplate layout;')
        choose_cols_used=scan(what=character(), n=ncol(df))
        print(choose_cols_used)
      }else{
        return(user_specific_labels)
      }
    }

  }
}
