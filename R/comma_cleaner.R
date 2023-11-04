#' Title: Comma Cleaner function.
#'
#' @description
#' This modular function, in the context of this package, is responsible for removing commas
#' from attribute(s) values. Removal of commas facilitates the conversion of attributes into the numeric class.
#'
#' @author Tingwei Adeck
#'
#' @param comma_df A dirty data frame obtained from the FLUOstar DAT file.
#'
#'
#' @return A clean data frame with numeric no-comma values for attribute(s).
#'
#' @export
#'
#' @seealso [clean_odd_cc()], [clean_odddat_optimus()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- comma_cleaner(dat_df)

comma_cleaner <- function(comma_df){
  cols_to_becleaned <- c(colnames(comma_df))
  comma_df[ , cols_to_becleaned] <- lapply(comma_df[ , cols_to_becleaned],
                                           function(x){ as.numeric(as.character(gsub(",", "", x))) })
  return(as.data.frame(comma_df))

}
