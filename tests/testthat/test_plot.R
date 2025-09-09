## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test normfluodbf filled wells plot", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  plate = setup_plate(init_plate())
  plate <- plate %>%
    upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
    run_steps %>% subset('A1,B1,C1,C9') %>%
    plot(whichplot = 1, legend_labels = c('beef_jerky','fatnose','yourmamasofat','youweird'))

  plt.obj = plate$fluostar_plot

  expect_false(is.null(plt.obj))
  expect_true(inherits(plate$fluostar_plot, c("ggplot", "gg", "ggplot2::ggplot")))
  expect_type(plate[["fluostar_plot"]], "S4") #S4 not list
  expect_length(as.list(plt.obj), as.integer(12)) #decompose gives 12 otherwise 1
})


# test_that("test side by side plot", {
#   lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
#   plate = setup_plate(init_plate())
#   plate <- plate %>%
#     upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
#     run_steps %>% subset(c('A1', 'B1', 'C1')) %>%
#     plot(whichplot = 4, legend_labels = c('test', 'negative', 'positive'))
#
#   expect_true(inherits(plate$sbs_plot, c("ggplot", "gg", "ggplot2::ggplot")))
# })

# test_that("test Plot Layout for subsetted wells -- match whichplot = 3 with subsetting for the layout", {
#   lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
#   plate = setup_plate(init_plate())
#   plate <- plate %>%
#     upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
#     run_steps %>%
#     subset_for_layout(c('A1', 'B1', 'C1','A2','B2','C2','A3','B3','C3','C12','C9')) %>%
#     plot(whichplot = 3)
#
#   #timeout issue
#   #expect_false(is.null(plate[["plate_layout"]]))
#   #expect_true(inherits(plate$plate_layout, c("plotly", "htmlwidget")))
# })
