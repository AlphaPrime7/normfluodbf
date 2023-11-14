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
#' @param df A data frame that requires attribute labels.
#' @param rows_used A character vector indicating the rows or tuples used on the microplate (usually a 96-well microplate). Initialized as NULL.
#' @param cols_used A numeric vector indicating the plate columns or attributes used. Initialized as NULL.
#'
#' @return Returns a character or numeric vector of attribute(s) names for the normalized data frame.
#'
#' @export
#'
#' @note This function was designed to avoid the use of stringr. This function is designed to
#' name attributes when the read direction is specified as horizontal.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' n = c('A','B','C')
#' sample_col_names <- dat_col_names_horizontal(resampled_scaled, n)

dat_col_names_horizontal <- function(df, rows_used=NULL,cols_used=NULL){

  cols_sort <- c()
  cols_sort_fit <- c()

  if(!is.null(cols_used) && !is.null(rows_used)){

    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,cols_used))
    }

    range <- 1:( length(df)/length(rows_used) )

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && !is.null(rows_used)){

    colnames_nocu <- c(1:ncol(df) )
    for(i in rows_used){
      cols_sort <- append(cols_sort, paste0(i,colnames_nocu))
    }

    range <- 1:( length(cols_sort)/ (length(rows_used)*length(rows_used)) )

    for(j in rows_used){
      increment = length(cols_sort)/length(rows_used)
      cols_sort_fit <- append(cols_sort_fit, cols_sort[range])
      range <- range + increment
    }
    return(cols_sort_fit)

  } else if(is.null(cols_used) && is.null(rows_used)){

    colnames_nocu <- c(1:ncol(df))
    return(colnames_nocu)
  }

}
