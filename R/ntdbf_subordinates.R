#' Title: A function to append a unique identifier attribute to any data frame within the normfluodbf package.
#'
#' @description
#' The function in the context of normfluodbf creates an attribute called Cycle_Number
#' and appends this attribute to the cleaned or wrangled data frame derived from the dirty DBF file.
#'
#' @author Tingwei Adeck
#'
#' @param df A data frame with 1:n number of rows.
#'
#' @return A data frame with the Cycle_Number attribute appended to the end of the data frame.
#'
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
#'
#' @description
#'A function that creates an attribute of seq(numrows) with a step size of 1, where the user provides the attribute name.
#'
#' @author Tingwei Adeck
#'
#' @param numrows The number of rows the user intends to have in the created data frame.
#'
#' @param col_name The desired attribute name.
#'
#' @return A user-named single attribute data frame with nrow = numrows.
#'
#' @export
#'
#' @examples generic_identifier(40, col_name="Cycle_No")

generic_identifier <- function(numrows, col_name){
  vect <- seq(numrows)
  df <- as.data.frame(vect)
  colnames(df) <- c(col_name)
  return(df)
}

#' Title: Min-Max normalization on a 0-1 scale.
#'
#' @author Tingwei Adeck
#'
#' @param x Attribute value(s).
#'
#' @return A normalized value (between 0 and 1) when applied to a single value or a normalized attribute with values between 0 and 1.
#'
#' @export
#'
#' @note The lapply function is required to apply this function across several attributes.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm)
#'
#' @references https://www.statology.org/how-to-normalize-data-in-r/

min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#' Title: Min-Max normalization on a 0-100 scale.
#'
#' @author Tingwei Adeck
#'
#' @param x Attribute value(s).
#'
#' @return A normalized value (between 0 and 100) when applied to a single value or a normalized attribute with values between 0 and 100.
#'
#' @export
#'
#' @note The lapply function is required to apply this function across several attributes.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], min_max_norm_percent)
#'
#' @references https://www.statology.org/how-to-normalize-data-in-r/

min_max_norm_percent <- function(x){
  ((x - min(x)) / (max(x) - min(x))) * 100
}

#' Title: Z-score standardization or normalization function.
#'
#' @author Tingwei Adeck
#'
#' @param x Attribute value(s).
#'
#' @return A standardized value (Z = N (0,1)) when applied to a single value or a standardized attribute with values (Z = N (0,1)).
#'
#' @export
#'
#' @note The lapply function is required to apply this function across several attributes.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], norm_z)
#'
#' @references https://www.statology.org/how-to-normalize-data-in-r/

norm_z <- function(x){
  (x - mean(x)) / sd(x)
}

#' Title: A decimal scaling function (a machine learning tool).
#'
#' @author Tingwei Adeck
#'
#' @param x Attribute value(s).
#'
#' @return A decimal scaled value when applied to a single value or a decimal scaled attribute(s).
#'
#' @export
#'
#' @note The lapply function is required to apply this function across several attributes.
#' This is NOT a normalization function, so data obtained from the decimal scaling function
#' exists on a sliding scale and SHOULD NOT be used for meaningful analysis.
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

#' Title: A log transformation function.
#'
#' @author Tingwei Adeck
#'
#' @param x Attribute value(s).
#'
#' @return A log-transformed value when applied to a single value or an attribute with log-transformed values.
#'
#' @export
#'
#' @note The lapply function is required to apply this function across several attributes.
#'
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], log_transformation)
#'
#' @references https://www.statology.org/how-to-normalize-data-in-r/

log_transformation <- function(x){
  log(x)
}

#' Title: A value rounding function.
#'
#' @description
#' Round attribute values to three decimal places.
#'
#'
#' @param x Attribute value(s).
#'
#' @return A rounded value with three decimal places when applied to a single value or an attribute with log-transformed values.
#'
#' @export
#'
#' @examples test_df <- as.data.frame(c(seq(40)))
#' colnames(test_df) <- "test"
#' test_df_norm <- lapply(test_df[1:ncol(test_df)], roundfluor)

roundfluor <- function(x){
  round(x, 5)
}
