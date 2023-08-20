#' Title: A modified unique identifier function for FLUOstar normalized data
#' @description
#' Package creates a column called Cycle_Number and adds to the cleaned dbf data frame
#'
#' @author Tingwei Adeck
#' @param df A data frame with n number of rows
#'
#' @return A new data frame with the Cycle_Number attribute added.
#' @export
#' @note The FLUOstar microplate reader runs in cycles with the number of cycles determined
#' by the experimenter. This function essentially counts the number of cycles and is a subordinate
#' function to the main function norm_tidy_dbf(). Also the function can be used as a standalone but the
#' only limitation is the column name will be Cycle_Number.
#' For a more generic version of this program, use the generic_identifier included.
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' unique_identifier(test_df)
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$Cycle_Number[i] <- x
  }
  return(df)
}

#' Title: A generic identifier similar to unique identifier but end users supply a column name.
#' @description
#' A function that creates a column 1:nrow(df) and steps by 1 but gives you the option to use any column name.
#'
#' @author Tingwei Adeck
#'
#' @param numrows The number of rows in a data frame of interest (nrows(df) can be used).
#' @param col_name The desired column name for the column.
#'
#' @return A new single column data frame with the desired attribute added.
#' @export
#'
#' @examples generic_identifier(40,col_name="Cycle_No")
generic_identifier <- function(numrows, col_name){
  vect <- seq(numrows)
  df <- as.data.frame(vect)
  colnames(df) <- c(col_name)
  return(df)
}

#' Title: Min-Max normalization of attributes that require normalization
#'
#' @author Tingwei Adeck (Adapted from Statology)
#' @param x A single value from an attribute passed in the function for normalization.
#'
#' @return A normalized value (value between 1 and 0)
#' @export
#' @note lapply is needed to apply the function across several columns in a data set.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm)
#' @references https://www.statology.org/how-to-normalize-data-in-r/
min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#' Title: Tidy and Normalize .dbf files obtained from experiments using the FLUOstar microplate reader.
#' @description
#' Normalize a tidy dbf data frame. Specific to FLUOstar dirty dbf files. Version 2 will also address .dat files.
#'
#'
#' @param file A string ("x.dbf) or path directly pointing to a .dbf file
#' @param fun A variable defined as NA, used for boolean expressions
#' @param ... A sequence of dots
#'
#' @import data.table
#' @import tidyr
#' @import foreign
#'
#' @return Normalized dataframe with a Time and Cycle_No column
#'
#' @export
#' @describe description
#'
#' @examples \dontrun{normalized_dbf <- norm_tidy_dbf(file= "C:/Users/GrandProf/Downloads/Repos_4cleanup/Repositories_AP7/normfluodbf/data-raw/liposomes_214.dbf")}
#'
norm_tidy_dbf <- function(file = NULL, fun = NA, ...){

  if(!is.null(file)){
    x <- foreign::read.dbf(file=file, as.is = F)
  } else {
    print("please enter a string for the .dbf file you want to normalize")
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
