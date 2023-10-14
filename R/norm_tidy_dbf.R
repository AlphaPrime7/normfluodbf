#' Title: A modified unique identifier function for FLUOstar normalized data
#' @description
#' The function creates a column called Cycle_Number and adds to the cleaned dbf data frame
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

#' Title: A function that performs Min-Max normalization(0-1 range) of attributes that require normalization
#'
#' @author Tingwei Adeck (Adapted from Statology)
#' @param x Column or attribute values passed into the min-max normalization function
#'
#' @return A normalized value (between 0 and 1) when used as a standalone function, or a normalized attribute(s) when used with lapply.
#' @export
#' @note The lapply function is required to apply the function across several columns in a data set.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm)
#' @references https://www.statology.org/how-to-normalize-data-in-r/

min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#' Title: A function that performs Min-Max normalization (0-100 range) of attributes that require normalization
#'
#' @author Tingwei Adeck
#' @param x Column or attribute values passed into the min-max normalization function
#'
#' @return A normalized value (between 0 and 100) when used as a standalone function, or a normalized attribute(s) when used with lapply.
#' @export
#' @note The lapply function is required to apply the function across several columns in a data set.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm_percent)
#' @references https://www.statology.org/how-to-normalize-data-in-r/

min_max_norm_percent <- function(x){
  ((x - min(x)) / (max(x) - min(x))) * 100
}

#' Title: A function that performs Z-score standardization (or normalization) of attributes
#'
#' @author Tingwei Adeck
#' @param x Column or attribute values passed into the Z-standardization function
#'
#' @return Attribute(s) with values that have mean 0 and standard deviation 1
#' @export
#' @note The lapply function is required to apply the function across several columns in a data set.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], norm_z)
#' @references https://www.statology.org/how-to-normalize-data-in-r/

norm_z <- function(x){
  (x - mean(x)) / sd(x)
}

#' Title: A function that performs decimal scaling (not normalization) of attributes
#'
#' @author Tingwei Adeck
#' @param x Column or attribute values passed into the decimal scaling function
#'
#' @return Attribute(s) with values that have the decimal place moved to facilitate machine learning
#' @export
#' @note The lapply function is required to apply the function across several columns in a data set.
#' This is NOT a normalization function because the data still exist on a sliding scale.
#' This function is here for demonstration purposes and should be used for exercise as it is here for educational purposes for the inventor of the package.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], decimal_scaling)
#' @references https://www.statology.org/how-to-normalize-data-in-r/

decimal_sacling <- function(x){
  max_abs <- max(abs(x))
  power <- ceiling(log10(max_abs))
  x/(10^power)
}

#' Title: A function that performs log transformation of data
#'
#' @author Tingwei Adeck
#' @param x Column or attribute values passed into the Z-standardization function
#'
#' @return Log transformed attributes
#' @export
#' @note The lapply function is required to apply the function across several columns in a data set.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], log_transformation)
#' @references https://www.statology.org/how-to-normalize-data-in-r/

log_transformation <- function(x){
  log(x)
}

#' Title: Cleans and Normalizes ".dbf" files obtained from experiments using the FLUOstar microplate reader.
#' @description
#' Input the path to a ".dbf" file obtained from the FLUOstar microplate (usually a 96-well microplate) reader; this function will create a data frame, clean the data frame, normalize the data frame, append a "Cycle_Number" column and return a data frame that is ready for analysis.
#' Most importantly, this function is a single_step function.
#' Also, the function can be extended to other ".dbf" files if they follow the format for which this function was designed; this is totally at the users' discretion.
#'
#'
#' @param file A string ("liposomes_xxx.dbf") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dbf" file, from FLUOstar experiments.
#' @param norm_scale This parameter can taken in 'hundred', 'one', 'z-score' or decimal, which denotes the normalization type; Initialized as NULL.
#' @param transformed This parameter can take in 'log' which denotes a logarithmic box-cox transformation; Initialized as NULL.
#' @param fun A variable defined as NA, used for boolean expressions or manipulation.
#' @param ... A container object that can be used to capture extra variables if needed.
#'
#' @importFrom data.table transpose
#' @import tidyr
#' @import foreign
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#'
#' @export
#' @note Re-nomenclature of norm_tidy_dbf to a more appropriate name that facilitates function utilization. Users can continue with the old name ("norm_tidy_dbf") but this is a better name in my opinion.
#' The default dbf normalization technique outputs values in the 0-1 range. The user needs to specify different aesthetics if they are used to seeing their plots on a different range.
#' The function accounts for nine conditions and there is an active quest to find a better way to write this.
#' Please NOTE that decimal scaling is a sliding scale (hypothesized) and so should yield unwanted results.
#' Please Note that the user must specify the norm_scale if they also want to specify log transformation(transformed argument0.
#'
#' @examples
#' fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' normalized_dbf <- norm_tidy_dbf(file=fpath)
#' normalized_dbf_scale100 <- norm_tidy_dbf(file=fpath, norm_scale = 'hundred')

norm_tidy_dbf <- function(file = NULL, norm_scale = NULL, transformed = NULL, fun = NA, ...){

  x <- foreign::read.dbf(file=file, as.is = F)

  if(is.null(file)){
    warning("please enter a string for the .dbf file you want to normalize")

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if (!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

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

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_sacling))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'one' && transformed == 'log'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'hundred' && transformed == 'log'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'z-score' && transformed == 'log'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'decimal' && transformed == 'log'){

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
    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_sacling))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if (!is.null(file)){

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

}
