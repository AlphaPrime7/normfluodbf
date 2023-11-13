#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description: Designed as a prototype function to take a single tuple or row
#' consisting of several samples and perform a putative resampling to yield
#' another data frame with a separate attribute for each sample.
#'
#' @author Tingwei Adeck
#'
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#'
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#'
#' @export
#'
#' @seealso [resample_dat_scale_alt()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odd_cc(dat_df)
#' col_1 <- nocomma_dat[,1]
#' col_1 <- as.data.frame(col_1)
#' samples_delineated <- resample_dat_alt(col_1, tnp=3, cycles=40)

resample_dat_alt <- function(df, tnp, cycles){

  suppressWarnings({

    k <- c(1:ncol(df))
    type_size <- c(1:ncol(df))

    resulting_df <- data.frame()
    for (i in 1:(nrow(df)/tnp)){

      insert_row = df[k,]

      resulting_df[i,type_size] <- rbind(insert_row, resulting_df)

      increment_k = tnp
      k <- k + increment_k

      colnames(resulting_df) <- NULL
    }
    return(resulting_df)

  })

}

