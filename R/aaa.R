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

Virtual_plate_maker = function(well_count = c(96L, 384L, 1536L)){

  if(96L %in% well_count){
     PLATE_META <-
     #another way to make data frames in R
       #default is A-H rows vs 1:12 cols
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else if (384L %in% well_count){
      #A-P rows vs 1:24 cols
      PLATE_META <-
      expand.grid(LETTERS[1:16], 1:24, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else if (1536L %in% well_count){
    PLATE_META <-
      #a little weird but A(4)-H(4) (A1,A2,A3,A4...)
      expand.grid(LETTERS[1:32], 1:48, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else {
    PLATE_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  }

  PLATE_META

}

