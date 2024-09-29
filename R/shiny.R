#' Run the shiny App
#' checking dependencies in R code ... WARNING
#' In addition to the functions provided in this package, the \code{normfluodbf} package
#' also provides an interactive tool that can be used to analyze liposome flux assay data
#' more easily. The tool will be launched in a web browser.
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "normfluodbf"),
                display.mode = "normal",
                launch.browser = TRUE)
}

#' Run the shiny App
#' In addition to the functions provided in this package, the \code{normfluodbf} package
#' also provides an interactive tool that can be used to analyze liposome flux assay data
#' more easily. The tool will be launched in a web browser.
#' @export
run_demo <- function() {
  shiny::runApp(system.file("shiny/demo", package = "normfluodbf"),
                display.mode = "normal",
                launch.browser = TRUE)
}

#' Run Demo Background
#' @param host localhost
#' @param appDir dir
#' @return NULL
#' @export
#' @examples
#' \dontrun {
#' demo_background(...)}
run_demo_bg <- function(host = getOption("shiny.host", "127.0.0.1"),
                            appDir = system.file("shiny/demo", package = "normfluodbf")) {

  if (!file.exists(appDir)) {
    stop("The Shiny app directory does not exist:", appDir)
  }

  tryCatch(
    {
      rstudioapi::verifyAvailable()
    },
    error = function(e) {
      #stopifnot(rstudioapi::hasFun("viewer"))
      message("RStudio API is not available. Using alternative method.")
    }
  )

  port <- shiny_random_port()

  run_demo_in_background(appDir = appDir, job_name = "normfluodbf_demo", host, port)

  viewerpane_background_normfluodbf(host, port)
}
