## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

library(testthat)

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test the %there% operator my first operator", {
  fpath <- system.file("extdata", package = "normfluodbf", mustWork = TRUE)
  x = "dat_1.dat" %there% fpath
  y = "dat_1.csv" %there% fpath
  expect_equal(x, TRUE)
  expect_equal(y, FALSE)
})
