#' Title: The visualizer function
#' @description
#' Visualizes liposome flux assay experiments for all three sample types
#'
#' @author Tingwei Adeck
#' @param df clean normalized dat data frame
#' @param x Cycle_No or Time
#' @param y_list sample types (tnp = test, negative, positive)
#' @param xlim x-axis range (0: number of cycles)
#' @param ylim  y-axis range (normalization range); 0:1 in this case
#'
#' @import ggplot2
#' @import ggthemes
#' @import ggdark
#'
#' @return A normalized data frame with the x-variable (Cycle_No), ready for analysis
#' @export
#' @note This is the MAIN function and stands alone but is dependent on the subordinate functions. If the user understands what they are doing this is all they need.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' n <- c('A','B','C')
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, n)
#' yvars <- c("A1","B1","C1")
#' xvar <- c("Cycle_No")
#' color <- c("Test","Negative Control","Positive Control")
#' xl <- c(0,40)
#' yl <- c(0,1)
#' gg_plot_triplets(normalized_fluo_dat,x=xvar,y_list=yvars,xlim=xl,ylim=yl)

gg_plot_triplets <- function(df, x, y_list, xlim, ylim){
  ggplot(df, aes(x=.data[[x[1]]])) +
    geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
    geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
    geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
    geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
    geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
    geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
    dark_mode()+  coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
    labs(title = 'NavAb Liposome Flux Assay',
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}


