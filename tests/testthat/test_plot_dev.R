## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test plot dev; the beefed up version of the original rookie function I used for Normfluodbf 1.5.2", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  wells <- normfluodat(lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'))
  plt.obj <- wells %>%
    subset_unplated_data(c('A1','B1','C1','A2')) %>%
    plot_dev_with_custom_legends(wells = c('A1','B1','C1','A2'), c('Navab', 'Kab', 'Clvab','Control'))
  expect_equal('plotly', class(plt.obj)[1])
  expect_equal(2, length(class(plt.obj)))
})
