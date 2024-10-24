## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test aru is not NULL", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = actual_rows_used(lipsum_214)
  expect_false(is.null(result))
})

test_that("test actual_cycles", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = actual_cycles(lipsum_214)
  cycles = 40
  expect_equal(result,cycles)
})

test_that("test aru output is accurate", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = actual_rows_used(lipsum_214)
  known_rows = c('A','B','C')
  expect_equal(result,known_rows)
})

test_that("test tnp", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = get_tnp(lipsum_214)
  tnp = 3
  expect_equal(result,tnp)
})
