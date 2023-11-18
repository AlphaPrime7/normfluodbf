#' Title: DAT file data frame cleaner.
#'
#' @description
#' The function takes the dirty data frame obtained from reading the FLUOstar DAT file, applies an original
#' algorithm that inserts NAs in place of the special characters, and then applies a function called
#' comma_cleaner() to the dirty data frame for the removal of commas, and finally, rows with NAs only are removed.
#'
#' @author Tingwei Adeck
#'
#' @param df A dirty data frame obtained from the FLUOstar DAT file.
#'
#'
#' @return A clean data frame with clean NA values retained.
#'
#' @export
#'
#' @seealso [comma_cleaner()], [clean_odd_cc()]
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odddat_optimus(dat_df)

clean_odddat_optimus <- function(df){

  suppressWarnings({

    special_chars <- c('-,','-' )
    empty_df <- data.frame()
    for (i in 1:nrow(df)){
      for (j in 1:ncol(df)){
        if( special_chars[1] %in% df[i,j] || special_chars[2] %in% df[i,j] ){
          df[i,j] <- NA
        }
      }
    }
    nona_rows_df <- df


    if(ncol(df) == 1){
      comma_df <- nona_rows_df
      comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]
      comma_df = as.numeric(as.character(gsub(",", "", comma_df)))

      return(as.data.frame(comma_df))

    } else {
      comma_df <- nona_rows_df
      nocomma_df <- nocomma_df[rowSums(is.na(nocomma_df)) != ncol(nocomma_df), ]
      nocomma_df <- comma_cleaner(comma_df)
      #nocomma_df <- as.numeric(nocomma_df)
      nocomma_df <- as.data.frame(nocomma_df)

      return(nocomma_df)
    }

  })

}
