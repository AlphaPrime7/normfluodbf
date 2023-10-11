#' Title: Tidy and Normalize .dbf files obtained from experiments using the FLUOstar microplate reader.
#' @description
#' Normalize a tidy dbf data frame. Specific to FLUOstar dirty dbf files. Version 2 will also address .dat files.
#'
#'
#' @param file A string ("x.dbf) or path directly pointing to a .dbf file
#' @param fun A variable defined as NA, used for boolean expressions
#' @param ... A sequence of dots
#'
#' @importFrom data.table transpose
#' @import tidyr
#' @import foreign
#'
#' @return Normalized dataframe with a Time and Cycle_No column
#'
#' @export
#' @note Simply renaming norm_tidy_dbf to a name that I like more. Users can continue with the old name but this is a better name in my opinion.
#'
#' @examples
#' fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' normalized_dbf <- normfluordbf(file=fpath)
normfluordbf <- function(file = NULL, fun = NA, ...){

  if(!is.null(file)){
    x <- foreign::read.dbf(file=file, as.is = F)
  } else {
    warning("please enter a string for the .dbf file you want to normalize")
  }

  y <- data.table::transpose(l=x)
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
  dbf_time_column <- data.frame()
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
