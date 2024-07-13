## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Comma Cleaner function.
#' @description
#' This modular function, in the context of this package, is responsible for removing commas
#' from attribute(s) values. Removal of commas facilitates the conversion of attributes into the numeric class.
#' @author Tingwei Adeck
#' @param comma_df A dirty data frame obtained from the FLUOstar DAT file.
#' @return A clean data frame with numeric no-comma values for attribute(s).
#' @export
#' @seealso [clean_odd_cc()], [clean_odddat_optimus()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- comma_cleaner(dat_df)
comma_cleaner <- function(comma_df){
  cols_to_becleaned <- c(colnames(comma_df))
  comma_df[ , cols_to_becleaned] <- lapply(comma_df[ , cols_to_becleaned],
                                           function(x){ as.numeric(as.character(gsub(",", "", x))) })
  return(as.data.frame(comma_df))

}

#' DAT file data frame cleaner.
#' @description
#' The function takes the dirty data frame obtained from reading
#' the FLUOstar DAT file and applies a function called comma_cleaner() to the dirty data frame,
#' which automatically inserts NAs in place of the special characters, and rows with NAs only are removed.
#' @author Tingwei Adeck
#' @import badger
#' @param df A dirty data frame obtained from the FLUOstar DAT file.
#' @return A clean data frame with clean NA values retained.
#' @export
#' @seealso [comma_cleaner()], [clean_odddat_optimus()]
#' @examples
#' fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' cleaned_dat <- clean_odd_cc(dat_df)
clean_commas <- function(df){
  suppressWarnings({

    df <- comma_cleaner(df)
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    return(df)
  })
}

#' DAT file wrangler.
#' @description
#' The function takes the dirty data frame obtained from reading the FLUOstar DAT file, applies an original
#' algorithm that inserts NAs in place of the special characters, and then applies a function called
#' comma_cleaner() to the dirty data frame for the removal of commas, and finally, rows with NAs only are removed.
#' @author Tingwei Adeck
#' @param df A dirty data frame obtained from the FLUOstar DAT file.
#' @return A clean data frame with clean NA values retained.
#' @export
#' @seealso [comma_cleaner()], [clean_odd_cc()]
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' partial_cleaned_dat <- clean_odddat_optimus(dat_df)
#' @rdname cleandats
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
      nocomma_df = as.numeric(as.character(gsub(",", "", comma_df)))

      return(as.data.frame(nocomma_df))

    } else {
      comma_df <- nona_rows_df
      comma_df <- comma_df[rowSums(is.na(comma_df)) != ncol(comma_df), ]
      nocomma_df <- comma_cleaner(comma_df)
      #nocomma_df <- as.numeric(nocomma_df)
      nocomma_df <- as.data.frame(nocomma_df)

      return(nocomma_df)
    }
  })
}
clean_odd_dat <- clean_odddat_optimus

#' DAT Wrangler
#' @author Tingwei Adeck
#' @param df df
#' @return df
#' @export
#' @seealso [comma_cleaner()], [clean_odd_cc()]
#' @examples \dontrun{clean_even_dat(df)}
#' @rdname cleandats
clean_even_dat <- function(df){
  for (i in (4 * (1:(nrow(df)/4)))){
    k <- seq(4)
    skip_values = 8 * seq(40)
    if (i %in% skip_values) next
    df[c(k + i, i),] <- NA
  }
  df <- na.omit(df)
  return(df)
}

