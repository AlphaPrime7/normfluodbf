## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Designed as a prototype function to take a single attribute or column
#' consisting of several samples and perform a putative resampling
#' to yield another data frame with a separate attribute for each sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param output A choice between "df' and 'vector' outputs. Leave NULL for a data frame.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_vect_scale()]
#' @note
#' This is the vectorized approach and should be a more efficient function when compared to say
#' @seealso [resample_dat()] or @seealso [resample_dat_alt()].
#' This function will produce a vertical layout as defined in this package.
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' samples_delineated <- resample_dat_vect(nocomma_dat, tnp=3, cycles=40)
#' @rdname resample
resample_dat_vect <- function(df, tnp, cycles, output=NULL){

  df_vector = as.vector(df)

  for (i in (1:length(df_vector)) ){
    list_len = i
    vec_len = length(df_vector[[i]])
  }
  vec_len
  list_len

  kth = 1
  k = df_vector[[list_len]] %>% .[c(kth)]

  #resulting_vec[1:tnp] <- c(list(), 'NULL')
  resulting_vec <- vector(mode = 'list', length = tnp * ncol(df))

  for(j in 1:(vec_len/tnp)){

    for (l in 1:length(resulting_vec)){

      inc_kth_by = 1
      k = df_vector[[list_len]] %>% .[c(kth)]
      resulting_vec[[l]] = c(resulting_vec[[l]], k)
      kth = kth + inc_kth_by
    }
  }
  if(is.null(output)|| output == 'df' ){

    resulting_df = as.data.frame(resulting_vec)
    #colnames(resulting_df) = rep('V', times = ncol(resulting_df))
    colnames(resulting_df) = c(1:ncol(resulting_df))
    return(resulting_df)
  } else {
    return(resulting_vec)
  }
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Designed as a prototype function to take a single attribute or column
#' consisting of several samples and perform a putative resampling
#' to yield another data frame with a separate attribute for each sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_scale()], [resample_dat_scale_optimus()]
#' @examples fpath <- system.file("extdata", "dat_5.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' samples_delineated <- resample_dat(nocomma_dat, tnp=3, cycles=40)
#' @rdname resample
resample_dat <- function(df, tnp, cycles){

  suppressWarnings({

    type_size <- c(1:tnp)
    k <- c(1:tnp)

    resulting_df <- data.frame()
    for (i in 1:(nrow(df)/tnp)) {

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

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description: Designed as a prototype function to take a single tuple or row
#' consisting of several samples and perform a putative resampling to yield
#' another data frame with a separate attribute for each sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_scale_alt()]
#' @examples fpath <- system.file("extdata", "dat_5.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' samples_delineated <- resample_dat_alt(nocomma_dat, tnp=3, cycles=40)
#' @rdname resample
resample_dat_alt <- function(df, tnp, cycles){

  suppressWarnings({

    k <- c(1:tnp)
    type_size <- c(1:tnp)

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

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is less optimized than @seealso [resample_dat_scale_optimus()].
#' @seealso [resample_dat()]
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat(df, tnp = tnp, cycles = cycles)
      return(df1)

    } else {

      col_list <- c()
      for(i in 1:ncol(df)){
        col_list <- c( col_list, as.data.frame(df[,i]) )
      }

      j_vect <- c()
      for(j in col_list){
        j <- as.data.frame(j)
        j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
        j_dfs <- as.data.frame(j_resampled)
        j_vect <- c(j_vect, j_dfs)
      }
      big_data = do.call(rbind, j_vect)
      big_data = as.data.frame(big_data)
      big_data_t = data.table::transpose(l=big_data)
      big_data_t <- big_data_t %>% dplyr::select_if(~ !any(is.na(.)))
      #big_data_t <- big_data_t[ , colSums(is.na(big_data_t))==0]
      big_data_t <- as.data.frame(big_data_t)

      return(big_data_t)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' NA values are retained for more control.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is less optimized than @seealso [resample_dat_scale_optimus()].
#' @seealso [resample_dat()]
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_naretainer(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_naretainer <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat(df, tnp, cycles)
      return(df1)

    } else {

      col_list <- c()
      for(i in 1:ncol(df)){
        col_list <- c( col_list, as.data.frame(df[,i]) )
      }

      j_vect <- c()
      for(j in col_list){
        j <- as.data.frame(j)
        j_resampled <- resample_dat(j, tnp = tnp, cycles = cycles)
        j_dfs <- as.data.frame(j_resampled)
        j_vect <- c(j_vect, j_dfs)
      }

      big_data = do.call(rbind, j_vect)
      big_data = as.data.frame(big_data)
      big_data_t = data.table::transpose(l=big_data)

      big_data_t <- big_data_t[ , colSums(!is.na(big_data_t))>=1]
      #big_data_t <-  big_data_t[, !sapply(big_data_t, function(x) all(x == 0))]
      #big_data_t <- big_data_t[,which(!apply(big_data_t,2,FUN = function(x){all(x == 0)}))]
      big_data_t <- as.data.frame(big_data_t)
      return(big_data_t)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' A function that takes tuples or rows consisting of several samples and perform a
#' putative resampling to yield another data frame with a separate attribute for each
#' sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param na_omit Takes a string "yes" OR "no".
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_alt()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_alt(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_alt <- function(df, tnp, cycles, na_omit = NULL){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df = resample_dat_alt(df,tnp,cycles)
      return(sc_df)

    } else {

      type_size <- c(1:ncol(df)) #k is now nseq(same kinda thing)

      sample_df <- data.frame()
      final_df <- matrix(ncol = cycles)

      for(i in 1:tnp){
        nseq <- c(i:ncol(df))
        nseq <- nseq

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,]
          sample_df[j,type_size] <- rbind(insert_row, sample_df)
          increment_n = tnp
          nseq <- nseq + increment_n

        }
        if(is.null(na_omit) || na_omit == 'no'){
          final_df <- cbind(final_df, sample_df)
          final_df <- final_df[ , colSums(!is.na(final_df))>=1]
          colnames(final_df) <- NULL
          #rownames(final_df) <- c(1:cycles)
          final_df <- as.data.frame(final_df)

        } else if(!is.null(na_omit) || na_omit == 'yes' ){
          final_df <- cbind(final_df, sample_df)
          final_df <- final_df[ , colSums(is.na(final_df))==0]
          colnames(final_df) <- NULL
          #rownames(final_df) <- c(1:cycles)
          final_df <- as.data.frame(final_df)
        }
      }
      return(final_df)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' A function that takes tuples or rows consisting of several samples and perform a
#' putative resampling to yield another data frame with a separate attribute for each
#' sample. NA values are retained.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_alt()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_alt_na(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_alt_na <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df = resample_dat_alt(df,tnp,cycles)
      return(sc_df)

    } else {

      type_size <- c(1:ncol(df)) #k is now nseq(same kinda thing)

      sample_df <- data.frame()
      final_df <- matrix(ncol = cycles)

      for(i in 1:tnp){
        nseq <- c(i:ncol(df))
        nseq <- nseq

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,]
          sample_df[j,type_size] <- rbind(insert_row, sample_df, na.omit=FALSE)
          increment_n = tnp
          nseq <- nseq + increment_n
        }
        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(!is.na(final_df))>=1]
        colnames(final_df) <- NULL
        rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
      }
      return(final_df)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' A function that takes tuples or rows consisting of several samples and perform a
#' putative resampling to yield another data frame with a separate attribute for each
#' sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_alt()], [resample_dat_scale_alt()]
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_alt_bf_na(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_alt_bf_na <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat_alt(df,tnp,cycles)
      return(df1)

    } else {

      row_list <- c()
      for(i in 1:nrow(df)){
        row_list <- c(row_list, as.data.frame(df[i,]) )
      }

      j_vect <- c()
      for(j in row_list){
        j <- as.data.frame(j)
        j_resampled <- resample_dat_alt(j, tnp = tnp, cycles = cycles)
        j_dfs <- as.data.frame(j_resampled)
        j_vect <- c(j_vect, j_dfs)
      }

      big_data = do.call(rbind, j_vect)
      big_data = as.data.frame(big_data)
      big_data_t = transpose(l=big_data)

      big_data_t <- as.data.frame(big_data_t)
      colnames(big_data_t) <- NULL

      num_samples = ncol(big_data_t)/cycles
      numrows = ncol(big_data_t)/num_samples
      k <- c(1:num_samples)
      type_size <- c(1:num_samples)

      resulting_df <- data.frame()
      for (i in 1:numrows){

        insert_col = big_data_t[,k]
        colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        colnames(resulting_df) <- c(1:num_samples)
      }
      #resulting_df <- resulting_df %>% dplyr::select_if(~ !any(is.na(.)))
      resulting_df <- resulting_df[ , colSums(!is.na(resulting_df))>=1]
      return(as.data.frame(resulting_df))
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' A function that takes tuples or rows consisting of several samples and perform a
#' putative resampling to yield another data frame with a separate attribute for each
#' sample.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_alt()], [resample_dat_scale_alt()]
#' @examples fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_alt_bfv(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_alt_bfv <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      df1 <- resample_dat_alt(df,tnp,cycles)
      return(df1)

    } else{

      row_list <- c()
      for(i in 1:nrow(df)){
        row_list <- c(row_list, as.data.frame(df[i,]) )
      }
      row_list = as.data.frame(row_list)
      big_data_t <- row_list
      colnames(big_data_t) <- NULL

      num_samples = ncol(big_data_t)/(cycles)
      numrows = ncol(big_data_t)/(num_samples)

      k <- c(1:num_samples)
      type_size <- c(1:num_samples)

      resulting_df <- data.frame()
      for (i in 1:numrows){

        insert_col = big_data_t[,k]
        colnames(insert_col) <- c(1:num_samples)

        resulting_df[i,type_size] <- rbind(insert_col, resulting_df)
        increment_k = num_samples
        k <- k + increment_k
        colnames(resulting_df) <- c(1:num_samples)

      }

      resulting_df <- resulting_df %>% dplyr::select_if(~ !any(is.na(.)))
      return(as.data.frame(resulting_df))
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is more optimized than @seealso [resample_dat_scale()], hence the suffix scale_optimus.
#' @seealso [resample_dat()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_optimus(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_optimus <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df = resample_dat(df,tnp,cycles)
      return(sc_df)

    } else {

      type_size <- c(1:tnp) #k is now nseq(same kinda thing)

      sample_df <- data.frame()
      final_df <- matrix(ncol = cycles)

      for(i in 1:ncol(df)){
        nseq <- c(1:tnp)

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,i]
          sample_df[j,type_size] <- rbind(insert_row, sample_df)
          increment_n = tnp
          nseq <- nseq + increment_n

        }

        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(is.na(final_df))==0]
        #final_df <- final_df %>% dplyr::select_if(~ !any(is.na(.)))
        colnames(final_df) <- NULL
        #rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
      }
      return(final_df)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' NA values are retained.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is more optimized than @seealso [resample_dat_scale()], hence the suffix scale_optimus.
#' @seealso [resample_dat()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_optimus_na(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_optimus_na <- function(df, tnp, cycles){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df <- resample_dat(df,tnp,cycles)
      return(sc_df)

    } else {

      type_size <- c(1:tnp) #k is now nseq(same kinda thing)

      sample_df <- data.frame()
      final_df <- matrix(nrow = cycles)

      for(i in 1:ncol(df)){
        nseq <- c(1:tnp)

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,i]
          sample_df[j,type_size] <- rbind(insert_row, sample_df, na.omit=FALSE)
          increment_n = tnp
          nseq <- nseq + increment_n

        }
        final_df <- cbind(final_df, sample_df)
        final_df <- final_df[ , colSums(!is.na(final_df))>=1]
        #final_df <- final_df[,which(!apply(final_df,2,FUN = function(x){all(x == 0)}))]
        #final_df <- final_df[, !sapply(final_df, function(x) all(x == 0))]
        #rownames(final_df) <- c(1:cycles)
        final_df <- as.data.frame(final_df)
        colnames(final_df) <- NULL
      }
      return(final_df)
    }
  })
}

#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param na_omit Takes a string "yes" OR "no".
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @note This function builds on or scales-up  @seealso [resample_dat()], hence the suffix scale.
#' This function is more optimized than @seealso [resample_dat_scale()], hence the suffix scale_optimus.
#' @seealso [resample_dat()]
#' @examples fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' resampled_scaled <- resample_dat_scale_optimus_backend(nocomma_dat, tnp=3, cycles=40)
#' @rdname resamplescale
resample_dat_scale_optimus_backend <- function(df, tnp, cycles, na_omit = NULL){

  suppressWarnings({

    if(ncol(df) == 1){

      sc_df = resample_dat(df,tnp,cycles)
      return(sc_df)

    } else {
      type_size <- c(1:tnp) #k is now nseq(same kinda thing)

      sample_df <- data.frame()
      final_df <- matrix(nrow = cycles)

      for(i in 1:ncol(df)){
        nseq <- c(1:tnp)

        for (j in 1:(nrow(df)/tnp)){

          insert_row = df[nseq,i]
          sample_df[j,type_size] <- rbind(insert_row, sample_df)
          increment_n = tnp
          nseq <- nseq + increment_n

        }

        if(is.null(na_omit) || na_omit == 'no'){
          final_df <- cbind(final_df, sample_df)
          final_df <- final_df[ , colSums(!is.na(final_df))>=1]
          colnames(final_df) <- NULL
          #rownames(final_df) <- c(1:cycles)
          final_df <- as.data.frame(final_df)

        } else if(!is.null(na_omit) || na_omit == 'yes' ){
          final_df <- cbind(final_df, sample_df)
          final_df <- final_df[ , colSums(is.na(final_df))==0]
          colnames(final_df) <- NULL
          #rownames(final_df) <- c(1:cycles)
          final_df <- as.data.frame(final_df)

        }
      }
      return(final_df)
    }
  })
}


#' A function to create an attribute or column for each sample loaded into the microplate wells.
#' @description
#' Creates a data frame where each sample loaded into the microplate wells has a separate attribute.
#' @author Tingwei Adeck
#' @param df A clean data frame with attributes or tuples containing a mixture of samples.
#' @param tnp A numeric value indicating the number of rows used. TNP is used as an acronym for Test, Negative, Positive.
#' @param cycles A numeric value indicating the number of cycles selected by the user when running the FLUOstar instrument.
#' @param method A string 'normal', 'brute' or 'vector' to specify the method of resampling.
#' @importFrom data.table transpose
#' @return A new data frame where separated samples are assigned a separate attribute or column.
#' @export
#' @seealso [resample_dat_vect()]
#' @note
#' This is the pseudo-vectorized approach and should be a more efficient function.
#' This function will produce a vertical layout as defined in this package.
#' This function inspired by the lapply approach pretty much applies the
#' @seealso [resample_dat_vect()]. As a matter of fact, I took this approach to
#' create compatibility with lapply and rapply but that failed.
#' @examples fpath <- system.file("extdata", "dat_3.dat", package = "normfluodbf", mustWork = TRUE)
#' dat_df <- read.table(file=fpath)
#' nocomma_dat <- clean_odddat_optimus(dat_df)
#' alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'brute')
#' alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'normal')
#' alt_test_scale <- resample_vect_scale(nocomma_dat,3,40, method = 'vector')
#' alt_test_scale_norm <- lapply(alt_test_scale, min_max_norm)
#' @rdname resamplescale
resample_vect_scale <- function(df, tnp, cycles, method = c('normal','brute', 'vector')){

  #df <- df[,colSums(is.na(df))<nrow(df)]
  df_vector = as.vector(df)

  total_list = c()
  for (i in (1:length(df_vector)) ){
    vec_len = length(df_vector[[i]])
    total_list = c(total_list, i)
  }
  vec_len
  total_list

  resulting_df = matrix()
  resulting_vect = c(list())
  j_vect <- c()
  dfbs = data.frame() #yes bs= bullshit

  if( is.null(method) || 'normal' %in% method){
    for(j in df_vector){
      j = as.data.frame(j)
      resampled_df = resample_dat_vect(j,tnp=tnp,cycles = cycles)
      colnames(resampled_df) = 'NULL'
      resulting_df = cbind(resulting_df, resampled_df)
    }

    resulting_df = resulting_df[,-1]
    resulting_df = as.data.frame(resulting_df)
    colnames(resulting_df) = c(1:ncol(resulting_df))
    return(resulting_df)

  } else if ('brute' %in% method){

    for(k in df_vector){
      k = as.data.frame(k)
      resampled_df = resample_dat_vect(k,tnp=tnp,cycles = cycles)
      j_vect <- c(j_vect, resampled_df)
    }
    dfbs = do.call(rbind, j_vect)
    dfbs = as.data.frame(dfbs)
    dfbs = data.table::transpose(l = dfbs)
    dfbs = as.data.frame(dfbs)
    return(dfbs)

  } else if ('vector' %in% method){
    for(l in df_vector){
      l = as.data.frame(l)
      resampled_vector = resample_dat_vect(l,tnp=tnp,cycles = cycles, output = 'vect')
      resulting_vect = append(resulting_vect, resampled_vector)
    }
    return(resulting_vect)
  }
}
