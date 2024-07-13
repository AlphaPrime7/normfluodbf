## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test normfluodat is not NULL", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = normfluodat(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A', 'B', 'C'))
  expect_false(is.null(result))
})

test_that("test normfluodat output length matches the number of cycles", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = normfluodat(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A', 'B', 'C'))
  cycles = 40
  expect_equal(nrow(result),cycles)
})

test_that("test normfluordat output length matches the number of cycles", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = normfluordat(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A', 'B', 'C'))
  cycles = 40
  expect_equal(nrow(result),cycles)
})

test_that("test normfluodatlite output length matches the number of cycles", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = normfluodatlite(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A', 'B', 'C'))
  cycles = 40
  expect_equal(nrow(result),cycles)
})

test_that("test normfluodatfull output length matches the number of cycles", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  result = normfluodatfull(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A', 'B', 'C'))
  cycles = 40
  expect_equal(nrow(result),cycles)
})
