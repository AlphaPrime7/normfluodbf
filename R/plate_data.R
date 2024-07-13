## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plate Data
#' @family platedata
#' @param plate plate
#' @param file file
#' @param tnp tnp
#' @param cycles cycles
#' @param rows_used rows_used
#' @param ... dots
#' @return plate data
#' @name platedata
#' @examples
#' \dontrun{plate_data(file, tnp, cycles, rows_used = c(A,B,C), norm_scale = 'raw')}
NULL

#' @rdname platedata
#' @return plate data
#' @export
plate_data = function(file, tnp = NULL, cycles = NULL, rows_used = NULL,...){
  if(grepl("\\.dbf$", file)){
    df = suppressMessages({normfluordbf(file, ...)})
    new_names = remove_leading_zero(names(df))
    colnames(df) <- new_names
    class(df) = c("normfluodbf_dbf", class(df))
    df
  }  else {
    if (is.null(tnp) || is.null(cycles))
      rlang::abort(sprintf('DAT:The params %s and %s must be provided and for making plates it is advised to provide %s', 'tnp', 'cycles', 'rows_used'));
    df = suppressMessages({normfluodat(file, tnp, cycles, rows_used, ...) })
    class(df) = c("normfluodbf_dat", class(df))
    df
  }
}

#' Format Plate Data
#' @family formatplatedata
#' @param plate plate
#' @return plate
#' @name formatplatedata
#' @examples
#' \dontrun{format_plate_data(plate)}
NULL

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data <- function(plate){
  UseMethod("format_plate_data")
}

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data.default = function(plate){
  CURRENT_STEP <- plate %>% step('FORMAT_DATA')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Formating Plate Data')
  phd = plate[['plate_data']]
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')

    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_time_cn(plate = plate, data = pfd)
    }

    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_cn(plate = plate, data = pfd)
    }
    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)
  }
}

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data.96well_plate = function(plate){

  CURRENT_STEP <- plate %>% step('FORMAT_DATA')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Formating Plate Data')
  phd = plate[['plate_data']]
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')

    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_time_cn(plate = plate, data = pfd)
    }

    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_cn(plate = plate, data = pfd)
    }
    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)
  }
}

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data.384well_plate = function(plate){
  CURRENT_STEP <- plate %>% step('FORMAT_DATA')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Formating Plate Data')
  phd = plate[['plate_data']]
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')

    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_time_cn(plate = plate, data = pfd)
    }

    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_cn(plate = plate, data = pfd)
    }
    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)
  }
}

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data.1536well_plate_t1 = function(plate){
  CURRENT_STEP <- plate %>% step('FORMAT_DATA')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Formating Plate Data')
  phd = plate[['plate_data']]
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')

    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_time_cn(plate = plate, data = pfd)
    }

    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_cn(plate = plate, data = pfd)
    }
    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)
  }
}

#' @rdname formatplatedata
#' @return plate
#' @export
format_plate_data.1536well_plate_t2 = function(plate){
  CURRENT_STEP <- plate %>% step('FORMAT_DATA')
  #plate %>% check_step(CURRENT_STEP)
  step_begin('Formating Plate Data')
  phd = plate[['plate_data']]
  phd %>% tibble::as_tibble()

  if (assertthat::are_equal(colnames(phd[,c(1,2)]), c("Time","Cycle_Number")) ){
    pfd = cbind( phd[1:2],
                 stack(phd[3:ncol(phd)]) )

    names(pfd)[3:ncol(pfd)] <- c('fluor_values','well')

    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_time_cn(plate = plate, data = pfd)
    }

    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)

  } else {
    pfd = cbind( phd[1],
                 stack(phd[2:ncol(phd)]) )
    names(pfd)[2:ncol(pfd)] <- c('fluor_values','well')
    if(is_normalized(pfd)){
      pfd = pfd %>%
        dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                      "well_col" = as.factor(gsub("[^0-9.-]", "", well)),
                      "sample" = well,
                      "used" = TRUE,
                      "outlier" = FALSE) %>%
        dplyr::select("well", "sample", "well_row", "well_col", "used", "Cycle_Number", "Time", "fluor_values", "outlier")
      message('Cannot check for outliers in already normalized plate data')
    }
    else {
      pfd = detect_outliers_cn(plate = plate, data = pfd)
    }
    load_plate_data(plate) = pfd
    status(plate) = define_status(plate)[['FORMAT_DATA']]
    steps(plate) = plate[['steps']][-1]
    step_end('Data Formatted')
    plate
    #return(pfd)
  }
}

#' Modify Plate Data
#' @family modifyplatedata
#' @param plate plate
#' @return plate
#' @name modifyplatedata
#' @examples
#' \dontrun{modify_plate_meta(plate)}
NULL

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data <- function(plate){
  UseMethod("modify_plate_data")
}

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data.default = function(plate){
  CURRENT_STEP <- plate %>% step('MODIFY_DATA')
  cols_to_remove = 'sample'; bycol = "well"
  jd <- dplyr::left_join(plate[['plate_meta']],
                         plate[['plate_data']],
                         suffix = c("",""), #can also use original vs new suffix
                         by = bycol) #multiple = "all" for full joins

      if(!is.null(cols_to_remove)){
        jd <- jd %>% dplyr::select(!all_of(c(cols_to_remove)))
        jd %<>% dplyr::mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
        jd %<>% dplyr::arrange(desc(used), well_row, well_col)
        jd = jd %>%
          dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                        "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
        class(jd) = c("mod_normfluodbf_data", class(jd))
        jd
        load_plate_data(plate) = jd
        status(plate) = define_status(plate)[['MODIFY_DATA']]
        steps(plate) = plate[['steps']][-1]
        plate

      }
      else {
        jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
        jd = jd %>%
          dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                        "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
        class(jd) = c("mod_normfluodbf_data", class(jd))
        jd
        load_plate_data(plate) = jd
        status(plate) = define_status(plate)[['MODIFY_DATA']]
        steps(plate) = plate[['steps']][-1]
        plate
      }
}

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data.96well_plate = function(plate){
  CURRENT_STEP <- plate %>% step('MODIFY_DATA')
  cols_to_remove = 'sample'; bycol = "well"
  jd <- dplyr::left_join(plate[['plate_meta']],
                         plate[['plate_data']],
                         suffix = c("",""), #can also use original vs new suffix
                         by = bycol
  ) #multiple = "all" for full joins

  if(!is.null(cols_to_remove)){
    jd <- jd %>% dplyr::select(!all_of(c(cols_to_remove)))
    jd %<>% dplyr::mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd %<>% dplyr::arrange(desc(used), well_row, well_col)
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate

  }
  else {
    jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate
  }
}

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data.384well_plate = function(plate){
  CURRENT_STEP <- plate %>% step('MODIFY_DATA')
  cols_to_remove = 'sample'; bycol = "well"
  jd <- dplyr::left_join(plate[['plate_meta']],
                         plate[['plate_data']],
                         suffix = c("",""), #can also use original vs new suffix
                         by = bycol
  ) #multiple = "all" for full joins

  if(!is.null(cols_to_remove)){
    jd <- jd %>% dplyr::select(!all_of(c(cols_to_remove)))
    jd %<>% dplyr::mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd %<>% dplyr::arrange(desc(used), well_row, well_col)
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate

  }
  else {
    jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate
  }
}

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data.1536well_plate_t1 = function(plate){
  CURRENT_STEP <- plate %>% step('MODIFY_DATA')
  cols_to_remove = 'sample'; bycol = "well"
  jd <- dplyr::left_join(plate[['plate_meta']],
                         plate[['plate_data']],
                         suffix = c("",""), #can also use original vs new suffix
                         by = bycol
  ) #multiple = "all" for full joins

  if(!is.null(cols_to_remove)){
    jd <- jd %>% dplyr::select(!all_of(c(cols_to_remove)))
    jd %<>% dplyr::mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd %<>% dplyr::arrange(desc(used), well_row, well_col)
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate

  }
  else {
    jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate
  }
}

#' @rdname modifyplatedata
#' @return plate
#' @export
modify_plate_data.1536well_plate_t2 = function(plate){
  CURRENT_STEP <- plate %>% step('MODIFY_DATA')
  cols_to_remove = 'sample'; bycol = "well"
  jd <- dplyr::left_join(plate[['plate_meta']],
                         plate[['plate_data']],
                         suffix = c("",""), #can also use original vs new suffix
                         by = bycol
  ) #multiple = "all" for full joins

  if(!is.null(cols_to_remove)){
    jd <- jd %>% dplyr::select(!all_of(c(cols_to_remove)))
    jd %<>% dplyr::mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd %<>% dplyr::arrange(desc(used), well_row, well_col)
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate

  }
  else {
    jd %<>% mutate(used = if_else(is.na(used), FALSE, TRUE), .keep = "all")
    jd = jd %>%
      dplyr::mutate("well_row" = gsub("[[:digit:]]", "", well),
                    "well_col" = as.factor(gsub("[^0-9.-]", "", well)))
    class(jd) = c("mod_normfluodbf_data", class(jd))
    jd
    load_plate_data(plate) = jd
    status(plate) = define_status(plate)[['MODIFY_DATA']]
    steps(plate) = plate[['steps']][-1]
    plate
  }
}

#' Upload Plate Data
#' @family uploadplatedata
#' @param plate plate
#' @param file file
#' @param ... dots
#' @return plate
#' @name uploadplatedata
#' @examples
#' \dontrun{upload_data(plate, file, ...)}
NULL

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data <- function(plate, file, ...){
  UseMethod("upload_data")
}

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data.default <- function(plate, file, ...){
  CURRENT_STEP <- plate %>% step('DATA_INITIALIZED')
  plate %>% check_step(CURRENT_STEP)
  step_begin('Initializing Data')
  pd = quiet({plate_data(file, ... )}, suppress_messages = T, suppress_warnings = T)
  #plate[['persistent_data']] <- pd
  wells_used = get_wells_used(pd)

  load_plate_data(plate) = pd
  plate = check_dirt(plate)
  plate[['dirty']] <- TRUE
  if (plate[['dirty']]){
    status(plate) = define_status(plate)[['DATA_INITIALIZED']] #length(steps(plate)) - (length(steps(plate)) - 1)
    steps(plate) = plate[['steps']][-1]
    file_name_without_ext <- tools::file_path_sans_ext(basename(file))
    plate[['dataset_name']] = file_name_without_ext
  }
  step_end('Data Initialized')

  plate
}

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data.96well_plate <- function(plate, file, ...){
  CURRENT_STEP <- plate %>% step('DATA_INITIALIZED')
  plate %>% check_step(CURRENT_STEP)
  step_begin('Initializing Data')
  pd = quiet({plate_data(file, ... )}, suppress_messages = T, suppress_warnings = T)
  #plate[['persistent_data']] <- pd
  wells_used = get_wells_used(pd)

  load_plate_data(plate) = pd
  plate[['dirty']] <- TRUE
  plate = check_dirt(plate)
  if (plate[['dirty']]){
    status(plate) = define_status(plate)[['DATA_INITIALIZED']]
    steps(plate) = plate[['steps']][-1]
    file_name_without_ext <- tools::file_path_sans_ext(basename(file))
    plate[['dataset_name']] = file_name_without_ext
  }
  step_end('Data Initialized')

  plate
}

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data.384well_plate <- function(plate, file, ...){
  CURRENT_STEP <- plate %>% step('DATA_INITIALIZED')
  plate %>% check_step(CURRENT_STEP)
  step_begin('Initializing Data')
  pd = quiet({plate_data(file, ... )}, suppress_messages = T, suppress_warnings = T)
  #plate[['persistent_data']] <- pd
  wells_used = get_wells_used(pd)

  load_plate_data(plate) = pd
  plate[['dirty']] <- TRUE
  plate = check_dirt(plate)
  if (plate[['dirty']]){
    status(plate) = define_status(plate)[['DATA_INITIALIZED']]
    steps(plate) = plate[['steps']][-1]
    file_name_without_ext <- tools::file_path_sans_ext(basename(file))
    plate[['dataset_name']] = file_name_without_ext
  }
  step_end('Data Initialized')

  plate
}

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data.1536well_plate_t1 <- function(plate, file, ...){
  CURRENT_STEP <- plate %>% step('DATA_INITIALIZED')
  plate %>% check_step(CURRENT_STEP)
  step_begin('Initializing Data')
  pd = quiet({plate_data(file, ... )}, suppress_messages = T, suppress_warnings = T)
  #plate[['persistent_data']] <- pd
  wells_used = get_wells_used(pd)

  load_plate_data(plate) = pd
  plate[['dirty']] <- TRUE
  plate = check_dirt(plate)
  if (plate[['dirty']]){
    status(plate) = define_status(plate)[['DATA_INITIALIZED']]
    steps(plate) = plate[['steps']][-1]
    file_name_without_ext <- tools::file_path_sans_ext(basename(file))
    plate[['dataset_name']] = file_name_without_ext
  }
  step_end('Data Initialized')

  plate
}

#' @rdname uploadplatedata
#' @return plate
#' @export
upload_data.1536well_plate_t2 <- function(plate, file, ...){
  CURRENT_STEP <- plate %>% step('DATA_INITIALIZED')
  plate %>% check_step(CURRENT_STEP)
  step_begin('Initializing Data')
  pd = quiet({plate_data(file, ... )}, suppress_messages = T, suppress_warnings = T)
  #plate[['persistent_data']] <- pd
  wells_used = get_wells_used(pd)

  load_plate_data(plate) = pd
  plate[['dirty']] <- TRUE
  plate = check_dirt(plate)
  if (plate[['dirty']]){
    status(plate) = define_status(plate)[['DATA_INITIALIZED']]
    steps(plate) = plate[['steps']][-1]
    file_name_without_ext <- tools::file_path_sans_ext(basename(file))
    plate[['dataset_name']] = file_name_without_ext
  }
  step_end('Data Initialized')

  plate
}

#' Load Plate Data
#' @family loadplatedata
#' @param plate plate
#' @param value data
#' @return plate
#' @name loadplatedata
#' @examples
#' \dontrun{load_plate_data(plate,value = data)}
NULL

#' @rdname loadplatedata
#' @return plate
#' @export
load_plate_data = function(plate){
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['plate_data']]
}

#' @rdname loadplatedata
#' @return plate
#' @export
`load_plate_data<-` <- function(plate, value) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['plate_data']] <- value
  plate
}
