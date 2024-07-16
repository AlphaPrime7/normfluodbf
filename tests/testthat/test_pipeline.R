## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

capture_output <- function(f, ...) {
  sink(tempfile())
   on.exit(sink())
   f(...)
 }

 test_that("test pipeline basics and determine the right step name function for dat files", {
   lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
   plate = setup_plate(init_plate(child_type = '96well_plate'))
   plate <- plate %>%
     upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw')
   expect_false(is.null(plate))
 })

 test_that("test pipeline basics and determine the right step name function for dbf files", {
   lipsum_214 <- system.file("extdata", "liposomes_214.dbf", package = "normfluodbf")
   plate = setup_plate(init_plate(child_type = '96well_plate'))
   plate <- plate %>%
     upload_data(file = lipsum_214, norm_scale = 'raw')
   expect_false(is.null(plate))
 })

 test_that("test pipeline with run steps", {
   lipsum_214 <- system.file("extdata", "dat_1.dat", package = "normfluodbf")
   plate = setup_plate(init_plate())
   plate <- plate %>%
     upload_data(file = lipsum_214, tnp = 3, cycles = 40, rows_used = c('A','B','C'), norm_scale = 'raw') %>%
     run_steps
   if (expect_false(is.null(plate))){
     message('The pipeline works')
   } else {
     warning('Investigate the pipeline')
   }

 })
