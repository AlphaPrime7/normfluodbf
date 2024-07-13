## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

library(testthat)

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test normfluodbf which is a modified version of my functions normfluordbf and norm_tidy_dbf", {
  lipsum_214 <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf")
  wells = normfluodbf(lipsum_214, norm_scale = 'one')
  expect_false(is.null(wells))
})
