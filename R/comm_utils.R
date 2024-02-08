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
