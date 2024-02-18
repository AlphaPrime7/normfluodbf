#' @export 
normfluodbf_warn_msg <- function(x) {
  warning(sprintf("normfluodbf: %s", x), call. = FALSE)
}

#' @export 
normfluodbf_stop_msg <- function(x) {
  stop(sprintf("normfluodbf: %s", x), call. = FALSE)
}

#' @export 
normfluodbf_msg_msg <- function(x) {
  message(sprintf("normfluodbf: %s", x), call. = FALSE)
}

#' Write a message to the user if the `normfluodbf.verbose` option is on
#'
#' Running a normfluodbf analysis results in many messages being printed to the console.
#' By default, these messages are on when the user is using R interactively
#' and off otherwise. You can overwrite this setting with \code{options(normfluodbf.verbose = FALSE)}.
#' @param ... Parameters to pass to \code{message()}
#' @keywords internal
#' @export
#' @example 

msg <- function(...) {
  if(isTRUE(getOption("normfluodbf.verbose", default = interactive()))) {
    message(...)
  }
}
