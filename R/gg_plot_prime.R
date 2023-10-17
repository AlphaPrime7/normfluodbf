#' Title: The visualizer function
#' @description
#' Visualizes liposome flux assay experiments for all three sample types
#'
#' @author Tingwei Adeck
#'
#' @import ggthemes
#'
#' @param df clean normalized dat data frame
#' @param x Cycle_No or Time
#' @param y_list sample types (tnp = test, negative, positive)
#' @param xlim x-axis range (0: number of cycles)
#' @param ylim  y-axis range (normalization range); 0:1 in this case
#'
#'
#' @return A normalized data frame with the x-variable (Cycle_No), ready for analysis
#' @export
#' @note This function is not very modular so use with caution. It should work just fine in most cases but here for demonstration.
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' fpath_dbf <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' n <- c('A','B','C')
#' normalized_fluo_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, n)
#' normalized_dbf_scalez <- norm_tidy_dbf(file=fpath_dbf, norm_scale = 'z-score')
#' yvars <- c("A1","B1","C1")
#' yvars_dbf <- c('A01','A02','A03')
#' xvar <- c("Cycle_Number")
#' color <- c("Test","Negative Control","Positive Control")
#' xl <- c(0,40)
#' yl <- c(0,1)
#' yl_dbf <- c(-3,3)
#' gg_plot_prime(normalized_fluo_dat,x=xvar,y_list=yvars,xlim=xl,ylim=yl)
#' gg_plot_prime(normalized_fluo_dat,xlim=xl,ylim=yl)
#' gg_plot_prime(normalized_dbf_scalez,x=xvar,y_list=yvars_dbf,xlim=xl,ylim=yl_dbf)

gg_plot_prime <- function(df, x=NULL, y_list=NULL, xlim, ylim){

  if(is.null(x) && is.null(y_list)){
    print('Enter the X-variable usually the cycle number')
    x_var = scan(what=character(), n=1)

    print('Enter the test sample')
    test = scan(what=character(), n=1)

    print('Enter the negative control')
    nc = scan(what=character(), n=1)

    print('Enter the positive control')
    pc = scan(what=character(), n=1)

    print('Enter the x-label')
    x_lab = scan(what=character(), n=1)
    print('Enter the y-label')
    y_lab = scan(what=character(), n=1)
    print('Enter the plot title')
    titlef = scan(what=character(), n=1)

    ggplot(df, aes(x=.data[[x_var]])) +
      geom_point(aes(y=.data[[test]]), size=3, color="blue") +
      geom_point(aes(y=.data[[nc]]), size=3, color="red") +
      geom_point(aes(y=.data[[pc]]), size=3, color="green") +
      geom_line(aes(y=.data[[test]], color="Test"), size = 0.8) +
      geom_line(aes(y=.data[[nc]], color="Negative Control"),size = 0.8) +
      geom_line(aes(y=.data[[pc]], color="Positivetive Control"), size = 0.8) +
      coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
      labs(title = titlef,
           x = x_lab , y= y_lab, color='Sample Type (TNP)')
  } else{
    print('Enter the x-label')
    x_lab = scan(what=character(), n=1)
    print('Enter the y-label')
    y_lab = scan(what=character(), n=1)
    print('Enter the plot title')
    titlef = scan(what=character(), n=1)

    ggplot(df, aes(x=.data[[x[1]]])) +
      geom_point(aes(y=.data[[y_list[1]]]), size=3, color="blue") +
      geom_point(aes(y=.data[[y_list[2]]]), size=3, color="red") +
      geom_point(aes(y=.data[[y_list[3]]]), size=3, color="green") +
      geom_line(aes(y=.data[[y_list[1]]], color="Test"), size = 0.8) +
      geom_line(aes(y=.data[[y_list[2]]], color="Negative Control"),size = 0.8) +
      geom_line(aes(y=.data[[y_list[3]]], color="Positivetive Control"), size = 0.8) +
      coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
      labs(title = titlef,
           x = x_lab , y= y_lab, color='Sample Type (TNP)')
  }
}
