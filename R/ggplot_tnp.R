#' Title: A Visualization function.
#'
#' @description
#' A visualization function using ggplot2.
#'
#' @author Tingwei Adeck
#'
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param x The X-variable often the cycle number OR time.
#' @param y_list A character vector of samples that need to be plotted. Often of the format TNP (Test, Negative, Positive).
#' @param xlim The X-variable scale.
#' @param ylim The Y-variable scale.
#'
#' @import ggplot2
#' @import ggthemes
#'
#' @return A nice visual of the clean and normalized data frame.
#'
#' @export
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- normfluodat(fpath,3,40, rows_used = c('A','B','C'), norm_scale = 'one')
#' yvars <- c("A1","B1","C1")
#' xvar <- c("Cycle_Number", "Time")
#' xl <- c(0,40)
#' yl <- c(0,1)
#' ggplot_tnp(dat_df,xvar,yvars,xl,yl)

ggplot_tnp <- function(df, x , y_list, xlim, ylim){
    p1 <- ggplot(df, aes(x=.data[[x[1]]])) +
      geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
      geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
      geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
      geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
      geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
      geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
      coord_cartesian( xlim = xlim, ylim = ylim ) +
      labs(title = 'NavAb Liposome Flux Assay',
           x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
    p1
}
