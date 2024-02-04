#' Parent plate type
#'
#' Assuming liposome flux assays were ran alongside a different experiment type,
#' I think a parent plate type will be handy in setting default parameters
#' for all other child plate types.
#'
#' @param plate An nfd plate
#'
#' @return The parent type of the given plate.
#' @export
#' @keywords internal

parent_plate_type <- function(plate) {
  UseMethod("parent_plate_type")
}


#' Parent plate type of default plates
#' @inheritParams parent_plate_type
#' @keywords internal

#' Plate type: nfd plate
#'
#' The default plate type that all other plates inherit from.
NULL
plate_types[['nfd_plate']] <- "nfd_plate"

parent_plate_type.nfd_plate <- function(plate) {
  # this is the default plate type -- there is no parent
  # Also the parent plate type is just specific to nfd.
  NULL

}

#' Plate types based on number of wells
#'
#' Liposome flux assays can be conducted in 96, 384 or 1536 microplates. \code{plate_types} simply
#' provides an empty list that can take any of the plate types above. If a plate type is
#' not specified then I assume a 96-well plate.

#' @format NULL
#' @usage NULL
#'
#' @examples
#' \dontrun{
#' plate_types()
#' }
#' @export
plate_types <- list()


#' S3 generics for choosing a plate type
#'
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for more
#' information on plate types.
#'
#' @param plate A  microplate
#'
#' @return The parent type of the given plate.
#'
#' @export
#'
#' @note
#' The plate type names are just the first letter of the numbers.
#' Also there are going to be 2 other plate types. These are
#' \code{tef_well} and \code{ofts_well}.
#'
#' @keywords internal

default_plate_type <- function(plate) {
  UseMethod("ns_well")
}

#' Define plate type parameters
#'
#' @details
#' Parameters in liposome flux assays are the same for all plates.
#' Parameters is entirely different from specificiations (specs). More on this
#' as I work out the details.
#'
#' @param plate A normfluodbf plate (\code{ns_well}, \code{tef_well} or \code{ofts_well} ).
#'
#' @return A list of parameters for the well chosen.
#'
#' @note
#' I do not know much about \code{ddpcr} but I know the plate parameters are same in
#' normfluodbf irrespective of 96 vs 384 vs 1536 well types. The default parameters
#' will be things like \code{MIN_FLUOR}, \code{MAX_FLUOR}, and \code{WELL_CAPACITY}.
#' There is a lot to learn here about R OOP and these S3 classes I had heard about.
#' Also \code{tidyqpcr} package will be important in learning more about R and how to
#' think when programming. These packages also introduce me to grids.
#'
#'
#' @export
#' @keywords internal

define_params <- function(plate) {
  UseMethod("define_params")
}


