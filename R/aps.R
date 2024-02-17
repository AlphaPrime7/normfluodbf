#' @title Empty Normfluodbf plate
#' @description
#' Not exported and for internal use only. See below for reference on my inspiration for this method.
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for more
#' information on plate types. It is obviously advanced R so I dont bank on casuals getting it
#' but the link should be a good starting point if interested. To understand it, the workflow must be
#' dissected surgically like I did before proper understanding of the topic can be obtained.
#' Despite grasping the topic, give it time to settle in and have more experience with R OOP.
#' Better yet, time can be circumvented by practicing and applying this idea to different problem types.
#' @details
#' This is an advanced plate setup vs a base plate setup (more on the base setup later).
#' This is the basis of forming an empty plate. If the user (often a non-casual) figures out how to setup a
#' plate without this internal function then they can circumvent it.
#' The next step is functionally defining each plate property listed here. I have a feeling
#' names and definitions are going to change when i start that.
#' @name empty_plate
NULL #documenting null

empty_plate <- function() {
  list(
    zero_plate = NULL,
    plate_data = NULL,
    plate_tech_meta = NULL, #vary by plate(contains plate technical specs different fluostar physics thresholds)
    plate_sci_meta = NULL, #parent type specific (init not null-define thresholds)
    steps = NULL, #init null plot steps happen eventually
  )
}

#' @title Global plate types list
#' These types of procedures need to be assigned to the global environment.
#' I think I saw a way this could be done and that will be implemented next.
#' For what its worth, this procedure will be devoted to a function of its own.
#' @name plate_types_global
NULL

plate_types <- list()
plate_types[['normfluodbf_plate']] <- "normfluodbfplate_plate"

#' @title Plate types tibble
#' @details
#' Just a static way of knowing the plate types. Honestly for normfluodbf this is about it
#' and it shouldn't get more complicated than this. The tibble begins with the parent plate type and
#' then follows with plate types indicating the number of wells in each.
#'
#' @return
#' A tibble
#'
#' @name plate_types_tbl
NULL
#' @seealso [plate_types()]

plate_types_tbl <- function(){

  # Make a plate type tibble
  # A list will also be made in another function as backup if issues occur
  # assigning the non-function() list to the global environment.
  plate_type_tbl <- tibble::tribble(
    ~plate_types, ~Value,
    "normfluodbf_plate", "normfluodbf_plate",
    "plate_96_wells", "plate_96_wells",
    "plate_384_wells", "plate_384_wells",
    "plate-1536_wells", "plate_1536_wells")

  class(plate_type_tbl) <- c("normfluodbf_plate_types", "advanced_plate_setup", class(plate_type_tbl))
  return(plate_type_tbl)
}

#' Plate types list
#' @details
#' The list equivalent of the tibble from \code{plate_types_tbl}.

plate_types <- function(){

    plate_types <- list()
    plate_types[['normfluodbf_plate']] <- "normfluodbf_plate"
    plate_types[["plate_96_wells"]]  <- "plate_96_wells"
    plate_types[['plate_384_wells']] <- "plate_384_wells"
    plate_types[['plate-1536_wells']] <- "plate_1536_wells"

    class(plate_types) <- c("normfluodbf_plate_types", "advanced_plate_setup", class(plate_types))
    return(plate_types)
}

#' @description
#' Parent plate type method.
#'
#' @author Tingwei Adeck
#'
#' @details
#' This is the method for setting the plate type.

parent_plate_type <- function(plate) {
  UseMethod("parent_plate_type")
}

#' @description
#' Parent plate type.
#' In short, no way to call a parent plate type for a class that does not exist.
#'
#' @author Tingwei Adeck
#'
#' @details
#' There is NO normfluodbf_plate class before this
#' so the use of this S3 method was deceptive to me at first
#' but after some frustration and thinking hard, i was able to
#' make sense of it.Once the plate is created then this class is assigned to that plate
#' and now an object with that class type exists hence why its called
#' the parent plate class.

parent_plate_type.normfluodbf_plate <- function(plate) {
  NULL
}

#' @description
#' Initializes the plate type once its created, even if empty.
#'
#' @author Tingwei Adeck
#'
#' @details
#' The default method should initialize the plate type for the newly created plate or
#' a plate without a plate type or one with plate_type "list" since the initial created plate is a list.
#'
parent_plate_type.default <- function(plate) {
  if(class(plate)[1] != "list" || class(plate)[1] == "normfluodbf_plate"){
    class(plate) <- class(plate)
    plate
  } else {
    class(plate) = c("normfluodbf_plate", class(plate))
  }
}

#' @description
#' Create a plate with class plate_96_wells.
#'
#' @author Tingwei Adeck
#'
#' @details
#' Once this plate type is specified by the user, there is nothing for this function to do.
#'
parent_plate_type.plate_96_wells <- function(plate) {
  NULL #nothing to do. The "plate_96_wells" class should not exist unless specified already.
}

#' @description
#' Initial step in setting up a plate is defining the type.
#' Shouldn't be too complicated with normfluodbf.
#' I need to determine the default parameters normfluodbf will want to use.
#'
#' @param plate A newly created plate (empty_plate is not exported)
#' @param type The plate type with default as normfluodbf_plate

setup_plate <- function(plate, type) {
  plate <- set_plate_type(plate, type)
  plate
}

#' @description
#' Define the plate type and often for an empty plate, it should be normfluodbf_plate.
#'
#' @param plate A newly created plate (empty_plate is not exported)
#' @param type The plate type with default as normfluodbf_plate
#'
#' @example
#' \dontrun{
#' set_plate_type(empty_plate())
#' }

set_plate_type <- function(plate, type) {
  #the plate type should be not be specified for the plate setup
  #otherwise simply return the specified plate type
  if (!missing(type) && !is.null(type) && class(plate)[1] != "list") {
    return(plate)
  }

  if (missing(type) || is.null(type) ) {
    type <- NULL
  }

  # Add the given type to the classlist (initially the class will be "list"
  # because that's the mode of a plate - we want to exclude that one)
  new_class <- type
  if (class(plate)[1] != "list") {
    new_class <- c(class(plate), new_class)
  }
  #structure(plate, class=new_class)
  class(plate) <- new_class
  plate

  set_plate_type(plate,
                 structure(plate, class = type) %>% parent_plate_type)
}



#' Add plate status
#'
#' @param plate A normfluodbf plate or empty plate
#'
#' @return
#' @export
#'
#' @examples
#' status(empty_plate())

status <- function(plate) {
  stopifnot(plate %>% inherits("normfluodbf_plate"))
  plate[['status']]
}


#' Empty plate check
#'
#' @param plate A normfluodbf plate or empty plate
#'
#' @return
#' @export
#'
#' @examples
#' is_plate_empty(empty_plate())

is_plate_empty <- function(plate) {
  is.null(status(plate))
}

#' @description
#' A new plate for now is just a function with no parameters that will create an empty plate from
#' scratch and assign a type for the plate.
#' Right now I am simply defining an empty plate and have not figured out the extra functionalities.
#'
#' @details
#' The process of making a new plate entails the following:
#' \code{empty_plate}
#' \code{setup_plate} <- \code{set_plate_type}
#'
#' @example
#' fpath <- system.file("extdata", "dat_2.dat", package = "normfluodbf", mustWork = TRUE)
#' new_plate(dat = fpath, tnp = 3, cycles = 40, rows_used = c('A','B','C'), type = NULL)
new_plate <- function(dat,
                      tnp,
                      cycles,
                      rows_used, type=NULL) {

  stopifnot(!is.null(rows_used))

  library(normfluodbf)
  plate = empty_plate()
  plate <- setup_plate(plate,type=type)
  status(plate)

  if(is_plate_empty(plate)){
    lipo_data = normfluodat(dat,tnp,cycles,rows_used)
    plate[["plate_data"]] <- lipo_data
    status(plate) = 'in_use'
    return(plate)
  } else {
    return(plate)
  }

}
