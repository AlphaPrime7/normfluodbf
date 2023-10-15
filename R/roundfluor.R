#' Title: Round attribute elements or attributes
#' @description
#' Round an attribute element or attribute.
#'
#'
#' @param x An attribute element that needs to be rounded up
#'
#' @return A rounded up element OR a rounded up attribute when assisted by lapply
#'
#' @export
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], roundfluor)

roundfluor <- function(x){
  round(x, 3)
}
