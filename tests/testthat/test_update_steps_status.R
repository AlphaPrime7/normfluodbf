## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_that("test updating steps and status", {
  plate = setup_plate(init_plate(child_type = '96well_plate'))
  plate <- plate %>%
    update_steps_list('REMOVE_OUTLIER', 'remove_outlier', 3) %>%
    update_status_list
  expect_false(is.null(plate))
})

test_that("test updating steps and status and ensuring the status is set to the current step", {
  plate = setup_plate(init_plate(child_type = '96well_plate'))
  plate <- plate %>%
    update_steps_list('REMOVE_OUTLIER', 'remove_outlier', 3) %>%
    update_status_list
  expect_equal(1,status(plate))
})

test_that("test pipeline modularity", {
  lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
  plate = setup_plate(init_plate(child_type = '96well_plate'))
  plate <- plate %>%
    update_steps_list('REMOVE_OUTLIERS', 'remove_outliers', 3) %>%
    update_status_list %>%
    upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
    run_steps
  expect_false(is.null(plate))
})
