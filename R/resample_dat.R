#' Title: A function to create an attribute or column for each sample loaded into the microplate wells.
#'
#' @description
#' Designed as a prototype function to take a single attribute or column
#' consisting of several samples and perform a putative resampling
#' to yield another data frame with a separate attribute for each sample.
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
#' @seealso [resample_dat_scale()], [resample_dat_scale_optimus()]
#'
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' col_1 <- nocomma_dat[,1]
#' col_1 <- as.data.frame(col_1)
#' samples_delineated <- resample_dat(col_1, tnp=3, cycles=40)

resample_dat <- function(df, tnp, cycles){

  suppressWarnings({

    type_size <- c(1:tnp)
    k <- c(1:tnp)

    resulting_df <- data.frame()
    for (i in 1:(nrow(df)/tnp)){

      colnames(resulting_df) = NULL
      insert_row = df[k,]
      colnames(insert_row) = NULL

      resulting_df[i,type_size] <- rbind(insert_row, resulting_df)

      increment = tnp
      k <- k + increment
    }
    return(resulting_df)

  })

}


