library(foreign)
library(ggplot2)
library(plotly)
library(tidyr)
library(plyr)

check_dbf <- function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.dbf$")) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.dbf$")
    return(files_list)
  } else{
    return("No .dbf files in pwd. Change the directory to one with .dbf files")
  }
}
#check_dbf(getwd())

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

#SUB FUNCTIONS
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$unique_id[i] <- x
  }
  return(df)
}


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
  
#FUNCTION START
dbf_wrangle <- function(x, fun = NA, ...){
  library(data.table)
  library(tidyr)
  y <- transpose(l=x)
  rownames(y) <- colnames(x)
  colnames(y) <- rownames(x) 
  colnames(y) <- paste0("a",rownames(x))

  sample_col_names<- vector("list")
  nofun <- is.na(fun)
  for(j in y[1,]){
    if(is.na(j) != nofun){
      sample_col_names <- c(sample_col_names,j)
    }
  }

  nofun <- is.na(fun) 
  dirty_time <- y[,1]
  dbf_time_column <- data.frame() #can be a matrix wrapped into a df
  for(i in dirty_time){
    if(is.na(i) != nofun && i != "t"){
      dbf_time_column <- rbind(dbf_time_column,i)
    }
  }
  colnames(dbf_time_column) <- c('Time')

  y[1:3,] <- NA
  y <- y %>% drop_na()
  y <- y[,-(1:2)]
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
  colnames(y) <- sample_col_names
  y <- cbind(y,dbf_time_column)
  y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
  y["Time"] = y[,"Time"] + 30
  return(unique_identifier(y))
  
}

#call function
# wrangled_dbf2 <- dbf_wrangle(dbf2)
# sapply(wrangled_dbf1, mode) #QC
# sapply(wrangled_dbf2, mode) #QC

#GGPLOT FUNCS()
# Reference:http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

#SUB
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## GGPLOT2

#shinyapp version 1 ).DATA[[]] VERSION
GG_plot_triplets <- function(df, x, y_list, xlim, ylim){
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
# 
# GG_plot_triplets(wrangled_dbf2,x=xvar,y_list=test,xlim=xl,ylim=yl)

#GET VERSION
GG_plot_triplets_get <- function(df, x, y_list, xlim, ylim){
  ggplot(df, aes(x=.data[[x[1]]])) + 
    geom_point(aes(y=get(y_list[1])), size=3, color="blue") +
    geom_point(aes(y=get(y_list[2])), size=3, color="red") +
    geom_point(aes(y=get(y_list[3])), size=3, color="green") +
    geom_line(aes(y=get(y_list[1]), color="Test"), size = 0.8) + 
    geom_line(aes(y=get(y_list[2]), color="Negative Control"),size = 0.8) + 
    geom_line(aes(y=get(y_list[3]), color="Positivetive Control"), size = 0.8) + 
    dark_mode()+  coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
    labs(title = 'NavAb Liposome Flux Assay', 
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}

# GG_plot_triplets_get(wrangled_dbf2,x=xvar,y_list=test,xlim=xl,ylim=yl)


#!!SYM VERSION
GG_plot_triplets_sym <- function(df, x, y_list, xlim, ylim){
  ggplot(df, aes(x=.data[[x[1]]])) + 
    geom_point(aes(y=!!sym(y_list[1])), size=3, color="blue") +
    geom_point(aes(y=!!sym(y_list[2])), size=3, color="red") +
    geom_point(aes(y=!!sym(y_list[3])), size=3, color="green") +
    geom_line(aes(y=!!sym(y_list[1]), color="Test"), size = 0.8) + 
    geom_line(aes(y=!!sym(y_list[2]), color="Negative Control"),size = 0.8) + 
    geom_line(aes(y=!!sym(y_list[3]), color="Positivetive Control"), size = 0.8) + 
    dark_mode()+  coord_cartesian(xlim = xlim) +  coord_cartesian(ylim=ylim) +
    labs(title = 'NavAb Liposome Flux Assay', 
         x = 'Cycle_no', y='Normalized Fluorescence', color='Sample Type')
}
# GG_plot_triplets(wrangled_dbf2,x=xvar,y_list=test,xlim=xl,ylim=yl)

## BASE PLOT

windows(width = 4.5, height = 4)
opar <- par(no.readonly = TRUE)
par(mar = c(4, 4, 3, 5)) #par(mar = c(5, 5, 4, 6))

plot.fun <- function(dbf_file){
  plot(A01 ~ Cycle_Number, 
       data = dbf_file, #normalized_dbf
       type = "o", 
       frame = T, 
       pch = 1, 
       col = "blue", 
       lwd = 2, 
       xlim = c(1,40), 
       main = "NavAb Liposome Flux Assay", 
       xlab = "Cycle No", 
       ylab = "Average Normalized Fluorescence")
  
  lines(A02 ~ Cycle_Number, 
        data = dbf_file, 
        pch = 2, 
        col = "red", 
        type = "o", 
        lty = 2, 
        lwd = 2)
  
  lines(A03 ~ Cycle_Number, 
        data = dbf_file, 
        pch = 7, 
        col = "green", 
        type = "o", 
        lty = 2, 
        lwd = 2)
  
  legend("topright", 
         inset = c(0.0, 0), 
         legend=c("Test", "NC","PC"), 
         title = "2uM ACMA (Cs inside)", 
         title.col = "black", 
         box.lwd = 1, 
         box.col = "orange", 
         col=c("blue","red","green"), 
         lty = 1:2, 
         lwd = 3, 
         cex=0.7, 
         xpd = T)
}
plot.fun(normalized_dbf)

### PLOTLY

plotly.fun<- function(dbf){
  fig1 <- plot_ly(dbf, x = ~Cycle_Number, y = ~A01, 
                  type = 'scatter', mode = 'markers', name = 'Test')
  fig1 <- fig1 %>% add_trace(y = ~A02, name = 'Negative Control')
  fig1 <- fig1 %>% add_trace(y = ~A03, name = 'Positive Control')
  fig1
  fig1 %>% layout(yaxis = list(range=c(-0.8,0), 
                               title = 'Log Normalized Fluorescence'),
                  title = 'NavAb Liposome Flux Assay')
  
}
plotly.fun(normalized_dbf)

## PLOT METHODS PRIOR (PLOTLY) - NONFUNCTIONAL
fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
normalized_dbf <- normfluordbf(file=fpath, norm_scale = 'one')
p1 = plot(A02 ~ Cycle_Number, data=normalized_dbf,
          pch=16,
          col="dodgerblue1",
          main = "NavAb Liposome Flux Assay",
          xlab = "Cycle Number",
          ylab = "Normalized Fluorescence")

plot(A03 ~ Cycle_Number, 
     data = normalized_dbf, 
     type = "b", 
     frame = FALSE, 
     pch = 19, 
     col = "red", 
     main = "NavAb Liposome Flux Assay", 
     xlab = "Cycle No", 
     ylab = "Normalized Fluorescence")

#END SUB
