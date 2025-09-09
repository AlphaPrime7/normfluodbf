## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

#' Plot Plate - Favorite is Fluostar style
#' @family normfluodbfplots
#' @param x plot requirement
#' @param whichplot int
#' @param fluorstarplot whichplot = 1
#' @param superimpose whichplot = 2
#' @param plate_layout whichplot = 3
#' @param plot_side_by_side whichplot = 4
#' @param legend_labels labels whichplot = 2,4
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
                             fluorstarplot = 1 %in% whichplot,
                             superimpose = 2 %in% whichplot,
                             plate_layout = 3 %in% whichplot,
                             plot_side_by_side = 4 %in% whichplot,
                             legend_labels = NULL,
                             plot_name = NULL,
                             ...){
  plate <- x
  plt.obj <- plot_canvas(plate)
  data <- subset_or_not(plate)
  title <- sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))
  ylab <- params(plate, 'GENERAL', 'Y_VAR')

  if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
  else if (superimpose){
    plot_superimpose_advanced(data = data,
                              plt.obj = plt.obj,
                              legend_labels = legend_labels)
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
                             ylab = ylab,
                             title = title,
                             legend_labels = legend_labels)
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
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
}

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.384well_plate = function(x,
                             whichplot = 1,
                             fluorstarplot = 1 %in% whichplot,
                             superimpose = 2 %in% whichplot,
                             plate_layout = 3 %in% whichplot,
                             plot_side_by_side = 4 %in% whichplot,
                             legend_labels = NULL,
                             plot_name = NULL,
                             ...){
  plate <- x
  plt.obj <- plot_canvas(plate)
  data <- subset_or_not(plate)
  title <- sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))
  ylab <- params(plate, 'GENERAL', 'Y_VAR')

  if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
  else if (superimpose){
    plot_superimpose_advanced(data = data,
                              plt.obj = plt.obj,
                              legend_labels = legend_labels)
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
                             ylab = ylab,
                             title = title,
                             legend_labels = legend_labels)
    plate <- remove_subset_data(plate)
    invisible(plate)
  }
  else {
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
}


#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.1536well_plate_t1 = function(x,
                             whichplot = 1,
                             fluorstarplot = 1 %in% whichplot,
                             superimpose = 2 %in% whichplot,
                             plate_layout = 3 %in% whichplot,
                             plot_side_by_side = 4 %in% whichplot,
                             legend_labels = NULL,
                             plot_name = NULL,
                             ...){
  plate <- x
  plt.obj <- plot_canvas(plate)
  data <- subset_or_not(plate)
  title <- sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))
  ylab <- params(plate, 'GENERAL', 'Y_VAR')

  if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
  else if (superimpose){
    plot_superimpose_advanced(data = data,
                              plt.obj = plt.obj,
                              legend_labels = legend_labels)
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
                             ylab = ylab,
                             title = title,
                             legend_labels = legend_labels)
    plate <- remove_subset_data(plate)
    invisible(plate)
  }
  else {
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
}

#' @rdname normfluodbfplots
#' @return print plot (return plate)
#' @export
plot.1536well_plate_t2 = function(x,
                                  whichplot = 1,
                                  fluorstarplot = 1 %in% whichplot,
                                  superimpose = 2 %in% whichplot,
                                  plate_layout = 3 %in% whichplot,
                                  plot_side_by_side = 4 %in% whichplot,
                                  legend_labels = NULL,
                                  plot_name = NULL,
                                  ...){
  plate <- x
  plt.obj <- plot_canvas(plate)
  data <- subset_or_not(plate)
  title <- sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))
  ylab <- params(plate, 'GENERAL', 'Y_VAR')

  if (is.null(whichplot) || is.null(fluorstarplot) || fluorstarplot){
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
  else if (superimpose){
    plot_superimpose(data = data,
                     ylab = ylab,
                     title = title,
                     legend_labels = legend_labels)
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
                             ylab = ylab,
                             title = title,
                             legend_labels = legend_labels)
    plate <- remove_subset_data(plate)
    invisible(plate)
  }
  else {
    plt.obj <- plot_grid(data, plt.obj)
    plt.obj <- plot_in_well(plt.obj, data, no_annotations = F)
    plate <- remove_subset_data(plate)
    if (is.null(plot_name))
      plot_name <- 'fluostar_plot'
    else
      plot_name <- plot_name
    plate[[plot_name]] <- plt.obj
    print(plt.obj)
    invisible(plate)
  }
}
