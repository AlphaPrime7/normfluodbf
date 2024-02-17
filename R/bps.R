#' Parent plate type
#'
#' Get used wells from the normalized data frame.
#'
#' @param plate A normalized normfluodbf data frame.
#' 
#' @import magrittr
#'
#' @return A tibble of used wells with a boolean attribute.
#' 
#' @export
#' 
#' @example 
#' fpath <- system.file("extdata", "dat_1.dat", package = "normfluodbf", mustWork = TRUE)
#' norm_dat <- normfluodat(dat=fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'))
#' norm_datt = data.table::transpose(norm_dat, keep.names = 'rn')
#' get_wells_used(norm_dat)
#' 

get_used_wells = function(norm_dat){

  pat = "^\\w{1-2}\\d+"
  wells_used = names(norm_dat)
  wells_used <- grep(pat,wells_used , value = TRUE)

  wells_used = tibble::as_tibble(wells_used) %>%
    magrittr::set_colnames(c('wells_used')) %>%
    dplyr::mutate(
      used = TRUE)

  wells_used

}

#' Blank Plate
#'
#' @author Tingwei Adeck
#' 
#' @import dplyr
#'
#' @param well_row Alphabet letters
#' @param well_col Numeric vector
#' 
#' @return A data frame or tibble with default plate data.
#'
#' @export
#' 
#' @examples
#' plate_meta = Virtual_plate_maker(well_row = LETTERS[1:8], well_col = 1:12)

create_blank_plate <- function(well_row = LETTERS[1:8], well_col = 1:12) {

  tidyr::crossing(well_row = as.factor(well_row),
                  well_col = as.factor(well_col)) %>%
    as_tibble() %>%
    tidyr::unite("well", well_row, well_col, 
                 sep = "", remove = FALSE)
}

#' PLATE(S) META
#'
#' @author Tingwei Adeck
#'
#' @param well_count Takes on value 96, 384 or 1536.
#'
#' @return A data frame or tibble with default plate data.
#'
#' @export
#'
#' @references https://www.statology.org/expand-grid-r/
#'
#' @examples
#' plate_meta = Virtual_plate_maker(well_count = 96L)

Virtual_plate_maker = function(well_count = c(96L, 384L, 1536L), well_type_1536 = NULL){

  if(96L %in% well_count){
     PLATE_META <-
     #another way to make data frames in R
       #default is A-H rows vs 1:12 cols
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                     "sample_type" = NA, "selected_wells" = NA,
                     "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else if (384L %in% well_count){
      #A-P rows vs 1:24 cols
      PLATE_META <-
      expand.grid(LETTERS[1:16], 1:24, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                     "sample_type" = NA, "selected_wells" = NA,
                     "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else if (1536L %in% well_count){

       if (well_type_1536 == 1){
        #for the Corning? and Roche Lightcycler (tm) 1536-well plates
        row_names = paste0(rep(LETTERS[1:8], each = 4), letters[1:4])
      } else {
        #for the Labcyte Echo 1536-well plates
        row_names = c(LETTERS[1:26], paste0("A", LETTERS[1:6]))
      }

    PLATE_META <-
      #a little weird but A(a)-H(d ) (Aa,Ab,Ac,Ad...)
      expand.grid(row_names, 1:48, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                     "sample_type" = NA, "selected_wells" = NA,
                     "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  } else {
    PLATE_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("well_row", "well_col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", well_row, well_col),
                     "sample_type" = NA, "selected_wells" = NA, #i assume selected_wells is used when working with a gui version of the well display.more on this.
                     "used_wells" = FALSE) %>%
      dplyr::select("well", "sample_type", "well_row", "well_col", "used_wells")

  }

  PLATE_META

}

