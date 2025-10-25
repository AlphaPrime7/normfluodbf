## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plot Plate - Favorite is Fluostar style
#' @family normfluodbfplots
#' @param x plot requirement
#' @param whichplot int
#' @param whichxvar int
#' @param fluorstarplot whichplot = 1
#' @param superimpose whichplot = 2
#' @param plate_layout whichplot = 3
#' @param plot_side_by_side whichplot = 4
#' @param Cycle_Number whichxvar = 1
#' @param Time whichxvar = 2
#' @param legend_labels labels whichplot = 2,4
#' @param whichpalette choose a palette using an integer 1-6
#' @param whichlinetype choose linetype 1-3
#' @param plot_name plot name
#' @param ... additional parameters
#' @return plot object
#' @name normfluodbfplots
#' @examples
#' \dontrun{plot(plate, whichplot = 1)}
NULL

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.96well_plate = function(x,
                             whichplot = 1,
                             whichxvar = NULL,
                             fluorstarplot = 1 %in% whichplot,
                             superimpose = 2 %in% whichplot,
                             plate_layout = 3 %in% whichplot,
                             plot_side_by_side = 4 %in% whichplot,
                             Time = 1 %in% whichxvar,
                             Cycle_Number = 2 %in% whichxvar,
                             legend_labels = NULL,
                             whichpalette = NULL,
                             whichlinetype = NULL,
                             plot_name = NULL,
                             ...){
  plate <- x
  plt.obj <- plot_canvas(plate, whichxvar = whichxvar)
  # !!important

  if("subset" %in% names(plate)){
    if(plate[['subset']] == TRUE){
      data = subset_or_not(plate)
    }
  }
  else {
    data = plate[['plate_data']]
  }

  suppressWarnings({
  if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){

      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plate,
                              plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)

  }
  else if (superimpose){
    plot_superimpose(plate,
                     data = data,
                     plt.obj = plt.obj,
                     legend_labels = legend_labels,
                     whichxvar = whichxvar,
                     whichpalette = whichpalette,
                     whichlinetype = whichlinetype)
    plate <- remove_legend(plate)
    plate <- remove_subset_data(plate)
    invisible(plate)
  }
  else if (plate_layout){
    plt.layout <- plot_plate_layout(plate)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'plate_layout'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.layout
    invisible(plate)
  }
  else if (plot_side_by_side){
    plt <- plot_side_by_side(plate,
                             data = data,
                             plt.obj = plt.obj,
                             whichxvar = whichxvar,
                             whichpalette = whichpalette,
                             whichlinetype = whichlinetype,
                             legend_labels = NULL)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'sbs_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    invisible(plate)
  }
  else {
      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plate,
                              plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)
  }
  })
}

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.384well_plate = function(x,
                             whichplot = 1,
                             whichxvar = NULL,
                             fluorstarplot = 1 %in% whichplot,
                             superimpose = 2 %in% whichplot,
                             plate_layout = 3 %in% whichplot,
                             plot_side_by_side = 4 %in% whichplot,
                             Time = 1 %in% whichxvar,
                             Cycle_Number = 2 %in% whichxvar,
                             legend_labels = NULL,
                             whichpalette = NULL,
                             whichlinetype = NULL,
                             plot_name = NULL,
                             ...){
  plate <- x
  plt.obj <- plot_canvas(plate, whichxvar = whichxvar)
  # !!important

  if("subset" %in% names(plate)){
    if(plate[['subset']] == TRUE){
      data = subset_or_not(plate)
    }
  }
  else {
    data = plate[['plate_data']]
  }

  suppressWarnings({
    if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){

      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)

    }
    else if (superimpose){
      plot_superimpose(data = data,
                       plt.obj = plt.obj,
                       legend_labels = legend_labels,
                       whichxvar = whichxvar,
                       whichpalette = whichpalette,
                       whichlinetype = whichlinetype)
      plate <- remove_legend(plate)
      plate <- remove_subset_data(plate)
      invisible(plate)
    }
    else if (plate_layout){
      plt.layout <- plot_plate_layout(plate)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'plate_layout'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.layout
      invisible(plate)
    }
    else if (plot_side_by_side){
      plt <- plot_side_by_side(data = data,
                               plt.obj = plt.obj,
                               whichxvar = whichxvar,
                               whichpalette = whichpalette,
                               whichlinetype = whichlinetype,
                               legend_labels = NULL)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'sbs_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      invisible(plate)
    }
    else {
      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)
    }
  })
}

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.1536well_plate_t1 = function(x,
whichplot = 1,
whichxvar = NULL,
fluorstarplot = 1 %in% whichplot,
superimpose = 2 %in% whichplot,
plate_layout = 3 %in% whichplot,
plot_side_by_side = 4 %in% whichplot,
Time = 1 %in% whichxvar,
Cycle_Number = 2 %in% whichxvar,
legend_labels = NULL,
whichpalette = NULL,
whichlinetype = NULL,
plot_name = NULL,
...){
  plate <- x
  plt.obj <- plot_canvas(plate, whichxvar = whichxvar)
  # !!important

  if("subset" %in% names(plate)){
    if(plate[['subset']] == TRUE){
      data = subset_or_not(plate)
    }
  }
  else {
    data = plate[['plate_data']]
  }

  suppressWarnings({
    if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){

      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)

    }
    else if (superimpose){
      plot_superimpose(data = data,
                       plt.obj = plt.obj,
                       legend_labels = legend_labels,
                       whichxvar = whichxvar,
                       whichpalette = whichpalette,
                       whichlinetype = whichlinetype)
      plate <- remove_legend(plate)
      plate <- remove_subset_data(plate)
      invisible(plate)
    }
    else if (plate_layout){
      plt.layout <- plot_plate_layout(plate)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'plate_layout'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.layout
      invisible(plate)
    }
    else if (plot_side_by_side){
      plt <- plot_side_by_side(data = data,
                               plt.obj = plt.obj,
                               whichxvar = whichxvar,
                               whichpalette = whichpalette,
                               whichlinetype = whichlinetype,
                               legend_labels = NULL)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'sbs_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      invisible(plate)
    }
    else {
      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)
    }
  })
}

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.1536well_plate_t2 = function(x,
                                  whichplot = 1,
                                  whichxvar = NULL,
                                  fluorstarplot = 1 %in% whichplot,
                                  superimpose = 2 %in% whichplot,
                                  plate_layout = 3 %in% whichplot,
                                  plot_side_by_side = 4 %in% whichplot,
                                  Time = 1 %in% whichxvar,
                                  Cycle_Number = 2 %in% whichxvar,
                                  legend_labels = NULL,
                                  whichpalette = NULL,
                                  whichlinetype = NULL,
                                  plot_name = NULL,
                                  ...){
  plate <- x
  plt.obj <- plot_canvas(plate, whichxvar = whichxvar)
  # !!important

  if("subset" %in% names(plate)){
    if(plate[['subset']] == TRUE){
      data = subset_or_not(plate)
    }
  }
  else {
    data = plate[['plate_data']]
  }

  suppressWarnings({
    if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){

      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)

    }
    else if (superimpose){
      plot_superimpose(data = data,
                       plt.obj = plt.obj,
                       legend_labels = legend_labels,
                       whichxvar = whichxvar,
                       whichpalette = whichpalette,
                       whichlinetype = whichlinetype)
      plate <- remove_legend(plate)
      plate <- remove_subset_data(plate)
      invisible(plate)
    }
    else if (plate_layout){
      plt.layout <- plot_plate_layout(plate)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'plate_layout'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.layout
      invisible(plate)
    }
    else if (plot_side_by_side){
      plt <- plot_side_by_side(data = data,
                               plt.obj = plt.obj,
                               whichxvar = whichxvar,
                               whichpalette = whichpalette,
                               whichlinetype = whichlinetype,
                               legend_labels = NULL)
      plate <- remove_subset_data(plate)
      if (is.null(plot_name))
        plot_name <- 'sbs_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      invisible(plate)
    }
    else {
      plt.obj <- plot_grid(data, plt.obj)
      plt.obj <- plot_in_well(plt.obj,
                              data,
                              whichxvar = whichxvar,
                              no_annotations = F)
      if (is.null(plot_name))
        plot_name <- 'fluostar_plot'
      else
        plot_name <- plot_name
      plate[[plot_name]] <- plt.obj
      plate <- remove_subset_data(plate)
      print(plt.obj)
      invisible(plate)
    }
  })
}

