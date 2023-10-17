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
