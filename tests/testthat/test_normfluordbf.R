## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

test_that("test normfluordbf", {
  lipsum_214 <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf")
  result = normfluordbf(lipsum_214)
  expect_false(is.null(result))
})
