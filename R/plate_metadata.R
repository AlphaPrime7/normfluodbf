## normfluodbf - R package for cleaning and normalizing liposome flux assay DBF and DAT files.
## Original Author: Dean Attali (ddpcr, 2015)
## Adapted Author: Tingwei Adeck
## Reference: https://www.statology.org/expand-grid-r/

# metadata representing a 96-well brand independent microplate.

virtual_plate_maker = function(well_count = c(96L, 384L, 1536L)){

  if(96L %in% well_count){
     WELL_META <-
     #another way to make data frames in R
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else if (384L %in% well_count){
      WELL_META <-
      expand.grid(LETTERS[1:16], 1:24, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else if (1536L %in% well_count){
    WELL_META <-
      #a little weird but A(4)-H(4) (A1,A2,A3,A4...)
      expand.grid(LETTERS[1:32], 1:48, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  } else {
    WELL_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate("well" = sprintf("%s%02d", row, col),
                     "sample" = NA, "selected_wells" = NA,
                     "used" = FALSE) %>%
      dplyr::select("well", "sample", "row", "col", "used")

  }

  WELL_META

}

