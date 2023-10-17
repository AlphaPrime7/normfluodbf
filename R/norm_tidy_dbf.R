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

decimal_scaling <- function(x){
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

#' Title: Cleans and Normalizes ".dbf" data files obtained from experiments using the FLUOstar Omega (Ω) microplate reader (from BMG LABTECH).
#' @description
#' The simplest function utilization scenario entails an input of the path to a ".dbf" file obtained from the FLUOstar microplate (usually a 96-well microplate) reader;
#' In a single step, this function will create a data frame, clean the data frame, normalize the data frame, append a "Cycle_Number" attribute,
#' perform an adjustment to the “time” attribute and return a data frame that is ready for analysis.
#' Since the initial publication of this package, several changes have been made to improve the user experience and to give the user more options to fine-tune the output from the package to meet the users’ aesthetic needs.
#' Users who decide to move past the simplest utility scenario have been given more options to customize the output based on the users’ needs.
#' Notably, several normalization sub-parameters have been provided in the package which yields different outputs based on what the user is used to seeing.
#' Just as the FLUOstar instrument is built to handle an array of assays,
#' this function is designed to be multi-dimensional (meaning it can handle data with the same DBF extension from other assay types),
#' on the condition that the data from assay types other than liposome flux assays follow the same data format this package was designed to handle.
#' Of course, users of this package are advised to pre-analyze DBF files from other assay types to ensure they are compliant with this package (compliance in this scenario is simple meaning DBF files from other assays should be like DBF files from liposome flux assays).
#'
#'
#' @param file A string ("liposomes_xxx.dbf") if the file is found within the present working directory (pwd) OR a path pointing directly to a ".dbf" file.
#' @param norm_scale This parameter takes sub-parameters: 'raw’ , hundred’ , 'one’ , 'z-score' , or 'decimal’ , which denotes the normalization type or scale; The parameter is initialized as NULL.
#' @param transformed This parameter takes input 'log', which denotes a logarithmic box-cox transformation; Initialized as NULL.
#' @param fun A parameter defined as NA is used for Boolean expressions or manipulation.
#' @param ... An abstract placeholder or container parameter that can be used to capture extra variables if needed.
#'
#' @importFrom data.table transpose
#' @import tidyr
#' @import foreign
#'
#' @return A normalized data frame with an appended "Cycle_Number" attribute.
#'
#' @seealso [normfluordbf()], [normfluordbf_legacy()], [normfluodat()]
#'
#' @export
#' @note
#' The default normalization sub-parameter outputs values in the 0-1 range.
#' Unless a “norm_scale” level is specified by the user, the default output is in the 0-1 range.
#' The “norm_scale” sub-parameter “decimal” is a machine-learning tool and should be avoided;
#' it also provides no advantage for basic research analysis as its output operates on a sliding scale just like the raw data.
#' Logarithmic transformation provides a minuscule advantage in data analysis and could/should be avoided.
#' Backward compatibility is maintained in all updates, so there should be no issues with using the package the way the user was used to.
#' The favorite "norm_scale" level is "z-score" since it divides the axis into negative and positive, thus facilitating interpretation.
#'
#' @examples
#' fpath <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf", mustWork = TRUE)
#' normalized_dbf_default <- norm_tidy_dbf(file=fpath)
#' normalized_dbf_scale100 <- norm_tidy_dbf(file=fpath, norm_scale = 'hundred')
#' normalized_dbf_scalez <- norm_tidy_dbf(file=fpath, norm_scale = 'z-score')
#' xvar <- c("Cycle_Number")
#' yvars_dbf <- c("A01","A02","A03")
#' xl <- c(0,40)
#' yl_scale100 <- c(0,100)
#' yl_default <- c(0,1)
#' yl_z <- c(3,-3)
#' color <- c("Test","Negative Control","Positive Control")
#' gg_plot_triplets(normalized_dbf_scale100,x=xvar,y_list=yvars_dbf,xlim=xl,ylim=yl_scale100)

norm_tidy_dbf <- function(file = NULL, norm_scale = NULL, transformed = NULL, fun = NA, ...){

  x <- foreign::read.dbf(file=file, as.is = F)
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
  fluor_threshold_check(y) #just polite friendly advise for the future

  if(is.null(file)){
    warning("please enter a string for the .dbf file you want to normalize")

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'raw'){

    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'hundred'){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if (!is.null(file) && !is.null(norm_scale) && norm_scale == 'one'){

    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'z-score'){

    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && norm_scale == 'decimal'){

    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'raw' && transformed == 'log'){

    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'one' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'hundred' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm_percent))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'z-score' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], norm_z))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if(!is.null(file) && !is.null(norm_scale) && !is.null(transformed) && norm_scale == 'decimal' && transformed == 'log'){

    y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
    y <- as.data.frame(lapply(y[1:ncol(y)], decimal_scaling))
    y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
    colnames(y) <- sample_col_names
    y <- cbind(y,dbf_time_column)
    y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
    y["Time"] = y[,"Time"] + 30

    return(unique_identifier(y))

  } else if (!is.null(file)){

    if(is.null(norm_scale) && !is.null(transformed) && transformed == 'log'){
      y <- as.data.frame(lapply(y[1:ncol(y)], log_transformation))
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30

      return(unique_identifier(y))
    } else {
      y <- as.data.frame(lapply(y[1:ncol(y)], min_max_norm))
      y <- as.data.frame(lapply(y[1:ncol(y)], roundfluor))
      colnames(y) <- sample_col_names
      y <- cbind(y,dbf_time_column)
      y[, c(1:ncol(y))] <- sapply(y[, c(1:ncol(y))], as.numeric)
      y["Time"] = y[,"Time"] + 30

      return(unique_identifier(y))

    }

  }

}
