library(ggplot2)
library(plotly)
#library(tidyr)
library(dplyr)

## --------LAZY DBFs LOADING-----------------------

# read_dbf <- function(dbf_list){
#   library(foreign)
#   dbffiles <- lapply(dbf_list,foreign::read.dbf)
#   return(dbffiles)
# }
# #dbfs <- read_dbf(list.files())
#
#
# for(i in 1:length(read_dbf(list.files()))){
#   n <- "dbf"
#   assign(paste0(n, i), as.data.frame(dbfs[i]))
# }

## --------Multiplot function(borrowed)-----------------------

# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots==1) {
#     print(plots[[1]])
#
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

## --------GGPLOT2 For Demo App (Super Rookie Mode)-----------------------

ggplot.normfluodbf.demo <- function(df,
                                    x,
                                    y_list,
                                    xlim = NULL,
                                    ylim = NULL) {
  # Ensure y_list contains exactly three elements
  if (length(y_list) != 3) {
    stop("y_list must contain exactly three elements.")
  }
  #get(#string) or !!sym(#string) for aes_string(); otherwise aes(y=.data[[string]])
  ggplot(df, aes_string(x = as.character(x)) ) +
    geom_point(aes_string(y = as.character(y_list[1])),
               color = "blue",
               shape = 16) +
    geom_point(aes_string(y = as.character(y_list[2])),
               color = "red",
               shape = 17) +
    geom_point(aes_string(y = as.character(y_list[3])),
               color = "green",
               shape = 18) +
    geom_line(aes_string(y = as.character(y_list[1])),
              color = "blue",
              linewidth = 0.8,
              linetype = "solid") +
    geom_line(aes_string(y = as.character(y_list[2])),
              color = "red",
              linewidth = 0.8,
              linetype = "dashed") +
    geom_line(aes_string(y = as.character(y_list[3])),
              color = "green",
              linewidth = 0.8,
              linetype = "dotted") +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    labs(title = 'NavAb Liposome Flux Assay',
         x = as.character(x),
         y = 'Normalized Fluorescence') +
    scale_color_manual(values = c("Test" = "blue",
                                  "Negative Control" = "red",
                                  "Positive Control" = "green")) +
    theme_minimal()
}


## --------BASE PLOT-----------------------

# windows(width = 4.5, height = 4)
# opar <- par(no.readonly = TRUE)
# par(mar = c(4, 4, 3, 5)) #par(mar = c(5, 5, 4, 6))
#
# plot.fun <- function(dbf_file){
#   plot(A01 ~ Cycle_Number,
#        data = dbf_file, #normalized_dbf
#        type = "o",
#        frame = T,
#        pch = 1,
#        col = "blue",
#        lwd = 2,
#        xlim = c(1,40),
#        main = "NavAb Liposome Flux Assay",
#        xlab = "Cycle No",
#        ylab = "Average Normalized Fluorescence")
#
#   lines(A02 ~ Cycle_Number,
#         data = dbf_file,
#         pch = 2,
#         col = "red",
#         type = "o",
#         lty = 2,
#         lwd = 2)
#
#   lines(A03 ~ Cycle_Number,
#         data = dbf_file,
#         pch = 7,
#         col = "green",
#         type = "o",
#         lty = 2,
#         lwd = 2)
#
#   legend("topright",
#          inset = c(0.0, 0),
#          legend=c("Test", "NC","PC"),
#          title = "2uM ACMA (Cs inside)",
#          title.col = "black",
#          box.lwd = 1,
#          box.col = "orange",
#          col=c("blue","red","green"),
#          lty = 1:2,
#          lwd = 3,
#          cex=0.7,
#          xpd = T)
# }
# plot.fun(normalized_dbf)

## --------PLOTLY-----------------------
#
# plotly.fun<- function(dbf){
#   fig1 <- plot_ly(dbf, x = ~Cycle_Number, y = ~A01,
#                   type = 'scatter', mode = 'markers', name = 'Test')
#   fig1 <- fig1 %>% add_trace(y = ~A02, name = 'Negative Control')
#   fig1 <- fig1 %>% add_trace(y = ~A03, name = 'Positive Control')
#   fig1
#   fig1 %>% layout(yaxis = list(range=c(-0.8,0),
#                                title = 'Log Normalized Fluorescence'),
#                   title = 'NavAb Liposome Flux Assay')
#
# }
# plotly.fun(normalized_dbf)
#
# ## PLOT METHODS PRIOR (PLOTLY) - NONFUNCTIONAL
# fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
# normalized_dbf <- normfluordbf(file=fpath, norm_scale = 'one')
# p1 = plot(A02 ~ Cycle_Number, data=normalized_dbf,
#           pch=16,
#           col="dodgerblue1",
#           main = "NavAb Liposome Flux Assay",
#           xlab = "Cycle Number",
#           ylab = "Normalized Fluorescence")
#
# plot(A03 ~ Cycle_Number,
#      data = normalized_dbf,
#      type = "b",
#      frame = FALSE,
#      pch = 19,
#      col = "red",
#      main = "NavAb Liposome Flux Assay",
#      xlab = "Cycle No",
#      ylab = "Normalized Fluorescence")
