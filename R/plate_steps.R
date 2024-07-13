## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Define Plate Steps
#' @family definesteps
#' @param plate plate
#' @param ... custome steps
#' @return NULL
#' @name definesteps
#' @examples
#' \dontrun{define_steps(plate)}
NULL

#' @rdname definesteps
#' @return NULL
#' @export
define_steps = function(plate){
  UseMethod('define_steps')
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.default <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.normfluodbf_plate <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.96well_plate <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.384well_plate <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.1536well_plate_t1 <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return steps
#' @export
define_steps.1536well_plate_t2 <- function(plate) {
  list(
    'NO_DATA' = 'no_data',
    'DATA_INITIALIZED' = 'upload_data',
    'FORMAT_DATA' = 'format_plate_data',
    'NORMALIZE' = 'normalize_by_well',
    'MODIFY_DATA' = 'modify_plate_data',
    'PLATE_READY' = 'analyze_ready'
  )
}

#' @rdname definesteps
#' @return plate
#' @export
set_default_steps <- function(plate, ...) {
  if (!missing(...)){
    steps(plate) %<>% utils::modifyList(steps)
  }

  steps(plate) <- define_steps(plate)
  plate
}

#' @rdname definesteps
#' @return plate
#' @export
#' @examples
#' \dontrun{plate <- plate %>% update_steps_list('REMOVE_OUTLIER', 'remove_outlier', 3)}
update_steps_list <- function(plate, new_key, new_value, index) {
  steps_list = steps(plate)

  steps_vector <- unlist(steps_list)
  new_entry <- setNames(new_value, new_key)
  updated_vector <- append(steps_vector, new_entry, after = index - 1)
  updated_steps <- as.list(updated_vector)

  plate[['steps']] <- updated_steps
  plate
}

#' Set Plate Steps
#' @family steps
#' @param plate plate
#' @param value value
#' @return plate
#' @name setsteps
#' @examples
#' \dontrun{steps(plate)}
NULL

#' @rdname setsteps
#' @return plate
#' @export
steps <- function(plate) {
  plate[['steps']]
}

#' @rdname setsteps
#' @return plate
#' @export
`steps<-` <- function(plate, value) {
  plate[['steps']] <- value
  plate
}

#' Steps Utils
#' @family steps
#' @param plate plate
#' @param step step
#' @return NULL
#' @name stepsutils
#' @examples
#' \dontrun{step(plate, step)}
NULL

#' @rdname stepsutils
#' @return step number
#' @export
step <- function(plate, step) {
  res <- plate %>% steps %>% names %>% {which(. == step)}
  res
}

#' @rdname stepsutils
#' @return step name
#' @export
step_name <- function(plate, step) {
  step %<>% as.integer
  plate %>% steps %>% names %>% .[step]
}

#' @rdname stepsutils
#' @return step name
#' @export
.step_name <- function(plate,step){
  step %<>% as.integer
  if (any(step < 1) || any(step > plate %>% steps %>% length)){
    message(sprintf("invalid step number: %s", paste(step, collapse = " ")))
  }
  plate %>% steps %>% names %>% .[step]
}

#' @rdname stepsutils
#' @return step name
#' @export
get_step_key_by_index <- function(steps, index) {
  if (index > length(steps) || index < 1) {
    return(NULL)
  } else {
    return(names(steps)[index])
  }
}

#' @rdname stepsutils
#' @return NULL
#' @note Step start time utilizing the cache
#' @export
step_begin <- function(...) {
  .globals$set("step_start", proc.time())
  message(sprintf("%s ...",...))
}

#' @rdname stepsutils
#' @return NULL
#' @note Step start time utilizing the cache
#' @export
step_end <- function(...) {
  message(sprintf("%s step DONE...", ...))
  message(sprintf("DONE (%s seconds)", round(proc.time() - .globals$get("step_start"))[1]))
}

#' @rdname stepsutils
#' @return boolean
#' @export
has_step <- function(plate,step){
  plate %>%
    steps %>%
    names %>%
    {which(. == step)} %>%
    length %>%
    magrittr::equals(1)
}

#' @rdname stepsutils
#' @return boolean
#' @export
check_step = function(plate, step){
  exists <- (plate %>% status)[step] >= step - 1
  if(exists){
    message(sprintf('The %s step needs to be completed', steps(plate)[[step]]))
  }
}

#' @rdname stepsutils
#' @return boolean
#' @export
steps_complete <- function(plate){
  length(steps(plate)) == 0
}

#' Steps Pipeline
#' @family stepspipeline
#' @param plate plate
#' @param n n
#' @param restart restart
#' @param ... dots
#' @return NULL
#' @name stepspipeline
#' @examples
#' \dontrun{next_step(plate, n=1)}
NULL

#' @rdname stepspipeline
#' @return plate
#' @note Recursive function to implement steps in the plate until all steps in the pipeline are complete
#' @export
.next_step <- function(plate, n = 1) {
  if (n == 0 || steps_complete(plate)) {
    if (steps_complete(plate)) {
      message("All steps completed")
    }
    return(plate)
  }

  current_status <- status(plate)
  next_step_name <- step_name(plate, ifelse(current_status == 0 || is.null(current_status), 1, current_status))

  next_step_fxn <- plate %>% steps %>% .[[next_step_name]]

  if (is.character(next_step_fxn) && exists(next_step_fxn, mode = "function")) {
    print(paste("Next step function:", next_step_fxn))
  }

  plate <- do.call(next_step_fxn, list(plate))
  next_step(plate, n - 1)
}

#' @rdname stepspipeline
#' @return plate
#' @note Recursive function to implement steps in the plate until all steps in the pipeline are complete
#' @export
next_step = function(plate,n=1){
  if (n == 0){
    return(plate)
  }

  if(steps_complete(plate)){
    message("All steps completed")
    return(plate)
  }

  next_step_name <- ""
  if(status(plate) == 0 || is.null(status(plate))){
    next_step_name <- step_name(plate, magrittr::add(status(plate), 1))
  }
  else if(status(plate) == 1){
    next_step_name <- step_name(plate, status(plate))
  }
  else {
    next_step_name <- step_name(plate,
                                magrittr::subtract(status(plate), (status(plate) - 1) ) )
  }
  next_step_name

  next_step_fxn <-
    plate %>%
    steps %>%
    .[[next_step_name]]

  if (is.character(next_step_fxn) && exists(next_step_fxn, mode = "function")) {
    print(paste("Next step function:", next_step_fxn))}

  plate <- do.call(next_step_fxn, list(plate))
  next_step(plate, n - 1)
}

#' @rdname stepspipeline
#' @return plate
#' @export
run_steps <- function(plate, reset = FALSE, ...){
  if(reset){
    message("Resetting the plate")
    plate <- reset_plate(plate)
    plate
  }
  else {
    steps_left = length(steps(plate))
    print(sprintf('Steps left:%d', steps_left))
    plate %<>% next_step(n = steps_left)
    message("Process complete")
    plate
  }
}
