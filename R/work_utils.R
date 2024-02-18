#' Step Start Time
#'
#' Learned that in writing R packages I can set a global variable by defining
#' a function to set and get values or parameters from the global environment.
#' @seealso \code{\link[normfluodbf]{step_end}}
#' @export
step_begin <- function() {
  .globals$set("step_tstart", proc.time())
  msg("Starting analysis", "... ", appendLF = FALSE)
}

#' Step End Time
#'
#' See the description of the lessons learned using this function in 
#' @seealso \code{\link[normfluodbf]{step_begin}}
#' @export
step_end <- function() {
  msg(sprintf("DONE (%s seconds)",
                  round(proc.time() - .globals$get("step_tstart"))[1]))
}

#' Get object class
#' 
#' Get the primary class of an object as the type.
#'
#' See the description of the lessons learned using this function in 
#' @export
type <- function(plate, all = FALSE) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  if (all) {
    class(plate)
  } else {
    class(plate)[1]
  }
}
