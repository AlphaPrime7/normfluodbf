#' Title: A function to get the actual columns used in the assay.
#'
#' @description
#' A function that facilitates a users' workflow by helping extract the actual columns used in the assay.
#'
#' @author Tingwei Adeck
#'
#' @param dat A string ("dat_1.dat") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dat" file.
#'
#' @return Returns a numeric vector denoting the columns used in the assay.
#'
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' acutest <- actual_cols_used(fpath)

actual_cols_used <- function(dat){
  df <- utils::read.table(dat)
  df <- clean_odddat_optimus(df)
  colnames(df) <- c(1:ncol(df))
  acu <- names(which(colSums(!is.na(df)) > 0))
  acu <- as.numeric(as.vector(acu))

  return(acu)

}
