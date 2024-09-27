## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test normfluodbf plot", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  plate = setup_plate(init_plate())
  plate <- plate %>%
    upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
    run_steps %>% subset('A1,B1,C1,C9') %>%
    plot(whichplot = 2, legend_labels = c('beef_jerky','fatnose','yourmamasofat','youweird'))

  glimpse(plate)
  plt.obj <- plot_superimpose(plate[['plate_data']])
  expect_equal("96well_plate" , class(plate)[1])
  expect_equal("gg", class(plt.obj)[1])
  expect_equal("ggplot", class(plt.obj)[2])
})

test_that("test Plot Layout for subsetted wells -- match whichplot = 3 with subsetting for the layout", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  plate = setup_plate(init_plate())
  plate <- plate %>%
    upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
    run_steps %>% subset_for_layout(c('A1', 'B1', 'C1','A2','B2','C2','A3','B3','C3','C12','C9')) %>% plot(whichplot = 3)

  ooh <- 'plate_layout'
  expect_equal("96well_plate", class(plate)[1])
  expect_equal("plotly", class(plate[[ooh]])[1])
  expect_equal("htmlwidget", class(plate[[ooh]])[2])
})

test_that("test side by side plot", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  plate = setup_plate(init_plate())
  plate <- plate %>%
    upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
    run_steps %>% subset(c('A1', 'B1', 'C1')) %>% plot(whichplot = 4, legend_labels = c('test', 'negative', 'positive'))

  expect_equal("96well_plate", class(plate)[1])
})
