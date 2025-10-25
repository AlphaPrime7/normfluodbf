## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test normfluodbf filled wells plot", {
  lipsum_214 <- system.file("extdata",
                            "dat_1.dat",
                            package = "normfluodbf")
  plate = setup_plate(init_plate())
  plate <- plate %>%
    upload_data(file = lipsum_214) %>%
    run_steps %>%
    subset('A1,B1,C1,C9') %>%
    plot(whichplot = 1, whichxvar = 2)

  plt.obj = plate$fluostar_plot

  expect_false(is.null(plt.obj))
  expect_true(inherits(plate$fluostar_plot, c("ggplot", "gg", "ggplot2::ggplot")))
})

