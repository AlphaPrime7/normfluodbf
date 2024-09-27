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
demo <- function() {
  shiny::runApp(system.file("demo", package = "normfluodbf"),
                display.mode = "normal",
                launch.browser = TRUE)
}
