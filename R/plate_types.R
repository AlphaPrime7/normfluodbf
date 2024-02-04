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
#' @keywords internal

default_plate_type <- function(plate) {
  #the way this is different from ddpcr is nfd plates do not need params or
  #really all i want them to be is empty as the parameter, but wait they do have
  #parameters, fluor values within (0-2^16), volume of solution it can take, likely a range of concs as well,
  #ok maybe more to think about here.
  UseMethod("96_well")
}

#' Define plate type parameters
#'
#' Parameters in liposome flux assays are the same for all plates unlike ddpcr.
#'
#' @param plate A normfluodbf plate
#'
#' @return A list o
#'
#' @note
#' I do not know much about \code{ddpcr} but I know the plate parameters are same in
#' normfluodbf irrespective of 96 vs 384 vs 1536 well types. The default parameters
#' will be things like \code{MIN_FLUOR}, \code{MAX_FLUOR}, and \code{WELL_CAPACITY}.
#' There is a lot to learn here about R OOP and these S3 classes I had heard about.
#' Also \code{tidyqpcr} package will be important in learning more about R and how to
#' think when programming.
#'
#' @export
#' @keywords internal

define_params <- function(plate) {
  UseMethod("define_params")
}


