#' Parent plate type of default plates
#' @inheritParams parent_plate_type
#' @keywords internal
parent_plate_type.ddpcr_plate <- function(plate) {
  # this is the default plate type -- there is no parent
  NULL
}

plate_types = list()
plate_types[['ddpcr_plate']] <- "ddpcr_plate"

