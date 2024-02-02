## normfluodbf - R package for cleaning and normalizing liposome flux assay DBF and DAT files.
## Original Author: Dean Attali (ddpcr, 2015)
## Adapted Author: Tingwei Adeck

# metadata representing a 96-well brand independent microplate.

virtual_plate_maker = function(well_count = NULL){

  if(well_count == 96L){
    WELL_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                     "sample" = NA, "target_ch1" = NA,  "target_ch2" = NA,
                     "used" = FALSE) %>%
      dplyr::select_("well", "sample", "row", "col", "used")

  } else if (well_count == 384L){
    WELL_META <-
      expand.grid(LETTERS[1:16], 1:24, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                     "sample" = NA, "target_ch1" = NA,  "target_ch2" = NA,
                     "used" = FALSE) %>%
      dplyr::select_("well", "sample", "row", "col", "used")

  } else if (well_count == 1536L){
    WELL_META <-
      #a little weird but A(4)-H(4) (A1,A2,A3,A4...)
      expand.grid(LETTERS[1:32], 1:48, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                     "sample" = NA, "target_ch1" = NA,  "target_ch2" = NA,
                     "used" = FALSE) %>%
      dplyr::select_("well", "sample", "row", "col", "used")

  } else {
    WELL_META <-
      expand.grid(LETTERS[1:8], 1:12, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(c("row", "col")) %>%
      dplyr::mutate_("well" = ~ sprintf("%s%02d", row, col),
                     "sample" = NA, "target_ch1" = NA,  "target_ch2" = NA,
                     "used" = FALSE) %>%
      dplyr::select_("well", "sample", "row", "col", "used")

  }

  WELL_META

}

