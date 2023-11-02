#' Title: A function to append a unique identifier attribute to any data frame within the normfluodbf package.
#' @description
#' The function in the context of normfluodbf creates an attribute called Cycle_Number
#' and appends this attribute to the cleaned or wrangled data frame derived from the dirty DBF file.
#'
#' @author Tingwei Adeck
#'
#' @param df A data frame with 1:n number of rows
#'
#' @return A data frame with the Cycle_Number attribute appended to the end of the data frame.
#' @export
#'
#' @seealso [normfluodat()], [norm_tidy_dbf()], [normfluordbf()], [generic_identifier()]
#'
#' @note The function operates in a closed system,
#' meaning it is primarily designed to work with this package ONLY.
#' Other use cases are simply a coincidence.
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

#' Title: A function to append a generic identifier attribute to any data frame, but users supply a name for said attribute.
#' @description
#'A function that creates an attribute of seq(numrows) with a step size of 1, where the user provides the attribute name.
#'
#' @author Tingwei Adeck
#'
#' @param numrows The number of rows the user intends to have in the created data frame.
#' @param col_name The desired attribute name.
#'
#' @return A user-named single attribute data frame with nrow = numrows.
#' @export
#'
#' @examples generic_identifier(40, col_name="Cycle_No")

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

#' Title: Round attribute elements or attributes
#' @description
#' Round an attribute element or attribute.
#'
#'
#' @param x An attribute element that needs to be rounded up
#'
#' @return A rounded up element OR a rounded up attribute when assisted by lapply
#'
#' @export
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], roundfluor)

roundfluor <- function(x){
  round(x, 3)
}
