#' Title: Cleans and Normalizes DBF files obtained from experiments using the FLUOstar microplate reader.
#'
#' @description
#' Input the path to a ".dbf" file obtained from the FLUOstar microplate (usually a 96-well microplate) reader;
#' this function will create a data frame, clean the data frame,
#' normalize the data frame, append a "Cycle_Number" column and
#' return a data frame that is ready for analysis.
#' Most importantly, this function is a single_step function.
#' Also, the function can be extended to other ".dbf" files if
#' they follow the format for which this function was designed;
#' this is totally at the users' discretion.
#'
#' @author Tingwei Adeck
#'
#' @param file A string ("liposomes_xxx.dbf") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dbf" file, from FLUOstar experiments.
#' @param norm_scale This parameter can taken in 'hundred', 'one', or 'z-score' which denotes the normalization type; Initialized as NULL.
#' @param transformed This parameter can take in 'log' which denotes a logarithmic box-cox transformation; Initialized as NULL.
#' @param fun A variable defined as NA, used for boolean expressions or manipulation.
#' @param ... A container object that can be used to capture extra variables if needed.
#'
#' @importFrom data.table transpose
#' @import data.table
#' @import tidyr
#' @import rio
#' @import foreign
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#'
#' @export
#'
#' @note Re-nomenclature of norm_tidy_dbf to a more appropriate name that facilitates function utilization. Users can continue with the old name ("norm_tidy_dbf") but this is a better name in my opinion.
#'
#' @examples
#' fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' normalized_dbf <- normfluordbf(file=fpath, norm_scale = 'raw')

normfluordbf <- function(file = NULL, norm_scale = NULL, transformed = NULL, fun = NA, ...){

  x <- foreign::read.dbf(file=file, as.is = F)
  #x <- rio::import(file)
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
  fluor_threshold_check(y) #just polite friendly QC

  if(is.null(file)){
    warning("please enter a string for the .dbf file you want to normalize")

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'raw'){

    if(is.null(transformed)){
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)

    } else{
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)
    }

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)
    }

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)
    }
  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

    if(is.null(transformed)){
      y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)

    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30
      y = unique_identifier(y)
      y = y %>% dplyr::relocate('Time', 'Cycle_Number')
      return(y)
    }
  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

      if(is.null(transformed)){
        y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
        y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
        colnames(y) <- sample_col_names
        y <- cbind(y,dbf_time_column)
        y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
        y["Time"] = y[,"Time"] + 30
        y = unique_identifier(y)
        y = y %>% dplyr::relocate('Time', 'Cycle_Number')
        return(y)

      } else{
        y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
        y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
        y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
        colnames(y) <- sample_col_names
        y <- cbind(y,dbf_time_column)
        y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
        y["Time"] = y[,"Time"] + 30
        y = unique_identifier(y)
        y = y %>% dplyr::relocate('Time', 'Cycle_Number')
        return(y)
      }

  } else if (!is.null(file)){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30
    y = unique_identifier(y)
    y = y %>% dplyr::relocate('Time', 'Cycle_Number')
    return(y)
  }
}
