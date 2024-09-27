## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

# --------------------- Plot Utils-Normfluodbf ---------------------------------

#' Adeckstats Plots
#' @family plotutils
#' @return plot utils
#' @name plotutils
#' @examples
#' \dontrun{
#' reverse_row(vect = vector)
#' column_levels(plate = plate)
#' row_levels(plate = plate)
#' base_plot(plate = plate)
#' save_plot(name = 'beefjerky')}
NULL

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
reverse_row <- function(vect) {
  vect = as.factor(vect) #levels(as.factor(vect))
  vect = rev(vect)
  vect
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
row_levels_layout <- function(plate) {
  data <- plate[['plate_data']]
  row_levels <-
    dplyr::pull(data, well_row) %>%
    as.factor() %>%
    factor(levels = rev(levels(.)))
  row_levels
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
row_levels_grid <- function(rows) {
  row_levels <-
    rows %>%
    as.factor() %>%
    factor(levels = levels(.))
  row_levels
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
column_levels <- function(plate) {
  col_levels <-
    dplyr::pull(plate[['plate_data']], well_col) %>%
    as.numeric() %>%
    as.factor() %>%
    levels() %>% as.numeric()
  col_levels
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
column_levels_layout <- function(plate) {
  data <- plate[['plate_data']]
  col_levels <-
    dplyr::pull(data, well_col) %>%
    as.character() %>%
    as.numeric() %>%
    factor(levels = sort(unique(.)))
  col_levels
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
column_levels_grid <- function(cols) {
  col_levels <-
    cols %>%
    as.character() %>%
    as.numeric() %>%
    factor(levels = sort(unique(.)))
  col_levels
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
row_levels <- function(plate) {
  rowlevels <-
    dplyr::pull(plate[['plate_data']], well_row) %>%
    as.factor() %>%
    levels()
  rowlevels #rev for reverse order(x[['plate_data']] %>% .[['well']] %>% as.vector() %>% unique())
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
get_x_var = function(data){
  xvar = ""
  for (i in names(data)){
    if (i == "Cycle_Number" && "Cycle_Number" %in% names(data)){
      xvar = i
    }
    else if (i == "Time" && "Time" %in% names(data)){
      xvar = i
    }
  }
  return(xvar)
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
get_y_var = function(data){
  yvar = ""
  for (i in names(data)){
    if (i == "fluor_values"){
      yvar = i
    }
  }
  return(yvar)
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
convert_x_one_label <- function(xlabel) {
  new_name <- tolower(xlabel)
  new_name <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", new_name, perl = TRUE)
  return(new_name)
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
convert_x_two_label <- function(xlabel) {
  new_name <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(xlabel), perl = TRUE)
  new_name <- gsub(" ", "_", new_name)
  return(new_name)
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
get_x_label <- function(xvar){
  if (xvar == "Cycle_Number"){
    xlab <- toupper(gsub("_", " ", xvar))
    xlab = as.character(xlab)
    return(xlab)
  }
  else {
    xlab <- gsub("Time", "TIME", xvar)
    xlab = as.character(xlab)
    return(xlab)
  }
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
add_custom_legend <- function(df, legend_values) {
  existing_wells <- unique(df$well)
  df <- df %>%
    mutate(legend = ifelse(well %in% existing_wells, legend_values[match(well, existing_wells)], "null"))
  return(df)
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
remove_legend <- function(plate) {
  data <- plate[['plate_data']]
  if ("legend" %in% colnames(data)) {
    data <- data %>% dplyr::select(-legend)
  } else {
    #nothing
    NULL
  }
  plate[['plate_data']] <- data
  plate
}

#' @rdname plotutils
#' @return plot utils
#' @note Just one of those functions I have found no use for
#' @keywords internal
reset_grid <- function(plate, grid_plot){
  bg_plot_r = "#DDDDDD"
  grid_plot <- grid_plot +
    ggplot2::geom_rect(
      data = plate[['plate_data']],
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = bg_plot_r
    )
  grid_plot
}

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
#' @export
save_plot <- function(name){
  ggplot2::ggsave(name, width=9,height=6)
}

# -------------------------------- Plot Handlers ----------------------------
#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_canvas <- function(plate){
  bg_plot_app = "transparent"
  bg_plot_r = "#DDDDDD"

  plt.obj <-
    ggplot2::ggplot() +
    ggplot2::xlab(x_var_two_label(plate)) +
    ggplot2::ylab(y_var(plate)) +
    ggplot2::ggtitle(sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 90, vjust = 0.5),
      title            = ggplot2::element_text(size = 14),
      axis.title       = ggplot2::element_text(size = 12),
      axis.text        = ggplot2::element_text(size = 12),
      strip.text       = ggplot2::element_text(size = 12),
      plot.background  = ggplot2::element_rect(fill = bg_plot_r, color = bg_plot_r),
      aspect.ratio     = NULL
    )
  plt.obj
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_in_well = function(plt.obj, data, no_annotations = FALSE){
  x = get_x_var(data)
  y = get_y_var(data)
  plt.obj <- plt.obj +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x, y),
      alpha = 0.3,
      size = 1
    ) + ggplot2::geom_line(data=data,ggplot2::aes_string(x, y), linewidth=0.1)

  if(no_annotations){
    plt.obj <- plt.obj +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank())
  }
  #plt.obj <- plotly::ggplotly(plt.obj)
  plt.obj
}

#' @rdname plotutils
#' @return ggplot list
#' @export
plot_grid <- function(data, plt.obj){
  plt.obj <- plt.obj +
    ggplot2::facet_grid(row_levels_grid(data$well_row) ~ column_levels_grid(data$well_col))
  plt.obj
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_grid_with_forcats = function(base_plot, data){
  p <- base_plot +
    ggplot2::facet_grid(forcats::fct_rev(data$well_row) ~ data$well_col,
                        space = "free_y",
                        switch = "x",
                        #labeller = labeller(well_row = reverse),
                        drop = FALSE) +
    theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
          strip.text.y = element_text(size=16, face="bold"),
          strip.background = element_rect(colour="red", fill="orange"))
  p
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_grid_alt.one = function(base_plot, data){
  p <- base_plot +
    ggplot2::facet_grid(reverse_row(data$well_row) ~ data$well_col,
                        space = "free_y",
                        switch = "x",
                        labeller = labeller(reverse_row(data$well_row)),
                        drop = FALSE) +
    theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
          strip.text.y = element_text(size=16, face="bold"),
          strip.background = element_rect(colour="red", fill="orange"))
  p
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_grid_alt.two = function(base_plot, data){
  p <- base_plot +
    ggplot2::facet_grid(factor(data$well_row, levels = reverse_row(data$well_row)) ~ data$well_col,
                        space = "free_y",
                        switch = "y",
                        drop = FALSE) +
    theme(strip.text.x = element_text(size=16, angle=0, face="bold"),
          strip.text.y = element_text(size=16, face="bold"),
          strip.background = element_rect(colour="red", fill="orange"))
  p
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_grid_alt.three = function(base_plot, data){
  p <- base_plot +
    ggplot2::facet_grid(reverse_row(data$well_row) ~ as.numeric(data$well_col),
                        space = "free_y",
                        switch = "both",
                        labeller = ggplot2::labeller(reverse_row(data$well_row)),
                        drop = FALSE) +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size=16, angle=0, face="bold"),
          strip.text.y = ggplot2::element_text(size=16, face="bold"),
          strip.background = ggplot2::element_rect(colour="gray", fill="gray"),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())
  p
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_superimpose <- function(data,
                             xlab = NULL,
                             ylab = NULL,
                             xlim=NULL,
                             ylim=NULL,
                             title=NULL,
                             legend_labels = NULL){
  if (!is.null(legend_labels)){
    data <- add_custom_legend(data, legend_values = legend_labels)
  }
  else{
    data <- data
  }

  x = get_x_var(data)
  y = get_y_var(data)
  if (is.null(xlab)) xlab = get_x_label(x)
  if (is.null(ylab)) ylab = 'FLUORESCENCE'
  if (is.null(title)) title <- sprintf('%s vs %s', ylab, xlab)
  well = 'well'
  legend = 'legend'
  if ("legend" %in% colnames(data)){
    plt.obj <- ggplot2::ggplot(data, ggplot2::aes_string(x=x, y=y, color=legend)) +
      ggplot2::geom_point(size=3) +
      ggplot2::geom_line(size=0.8) +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim) +
      ggplot2::labs(title=title, x=xlab, y=ylab, color='legend') +
      ggplot2::theme_minimal()
    print(plt.obj)
    return(plt.obj)
  }
  else {
    plt.obj <- ggplot2::ggplot(data, ggplot2::aes_string(x=x, y=y, color=well)) +
      ggplot2::geom_point(size=3) +
      ggplot2::geom_line(size=0.8) +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim) +
      ggplot2::labs(title=title, x=xlab, y=ylab, color='Sample Type') +
      ggplot2::theme_minimal()
    print(plt.obj)
    return(plt.obj)
  }
}

#' @rdname plotutils
#' @return ggplot list
#' @note purely subordinate because of the plt.obj found in the plot function
#' @keywords internal
plot_superimpose_advanced <- function(data,
                                      plt.obj,
                                      xlab = NULL,
                                      ylab = NULL,
                                      title = NULL,
                                      legend_labels = NULL) {
  if(!is.null(legend_labels))
    data <- add_custom_legend(data,legend_labels)
  else
    data <- data

  sample_cols <- unique(data$well)
  xvar <- get_x_var(data)
  yvar <- get_y_var(data)
  well = 'well'
  legend = 'legend'
  if (is.null(xlab)) xlab = get_x_label(xvar)
  if (is.null(ylab)) ylab = 'FLUORESCENCE'
  if (is.null(title)) title <- sprintf('%s vs %s', ylab, xlab)

  if("legend" %in% colnames(data)){
    plt.obj <- plt.obj + ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x = xvar, y = yvar, color = legend),
      size = 1,
      show.legend = TRUE
    ) +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes_string(x = xvar, y = yvar, group = legend, color = legend),
        size = 0.8,
        alpha = 0.7
      ) +
      ggplot2::labs(title=title, x=xlab, y=ylab, color = legend)
  }
  else {
    plt.obj <- plt.obj + ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x = xvar, y = yvar, color = well),
      size = 1,
      show.legend = TRUE
    ) +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes_string(x = xvar, y = yvar, group = well, color = well),
        size = 0.8,
        alpha = 0.7
      ) +
      ggplot2::labs(title=title, x=xlab, y=ylab, color = well)

  }

  if ("legend" %in% colnames(data)) {
    legend_labels <- unique(data$legend)
    plt.obj <- plt.obj + ggplot2::scale_color_manual(
      name = 'Well',
      values = wesanderson::wes_palette(n = length(sample_cols), name = 'Zissou1', type = 'continuous'),
      breaks = sample_cols,
      labels = legend_labels
    )
  }
  else {
    plt.obj <- plt.obj + ggplot2::scale_color_manual(
      name = 'Well',
      values = wesanderson::wes_palette(n = length(sample_cols), name = 'Zissou1', type = 'continuous'),
      breaks = sample_cols
    )
  }
  plt.obj <- plotly::ggplotly(plt.obj)
  print(plt.obj)
  return(plt.obj)
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_plate_layout <- function(plate){
  data <- subset_or_not(plate)
  plt.obj <-
    ggplot2::ggplot(data, ggplot2::aes(column_levels_layout(plate), row_levels_layout(plate) )) +
    ggplot2::geom_tile(ggplot2::aes(fill = used), color = "#222222", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#333333", "FALSE" = "white")) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      line             = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(size = 15, color = "blue"),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_fixed()
  plt.obj <- plotly::ggplotly(plt.obj)
  print(plt.obj)
  return(plt.obj)
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_return_plot.obj = function(data, plt.obj){
    plt.obj <- plot_grid(data = data, plt.obj = plt.obj)
    plt.obj = plot_in_well(plt.obj, data, no_annotations = F)
    print(plt.obj)
    plt.obj
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_side_by_side <- function(data,
                              plt.obj,
                              xlab = NULL,
                              ylab = NULL,
                              xlim=NULL,
                              ylim=NULL,
                              title=NULL,
                              legend_labels = NULL){
  x = get_x_var(data)
  y = get_y_var(data)
  if (is.null(xlab)) xlab = get_x_label(x)
  if (is.null(ylab)) ylab = 'FLUORESCENCE'
  if (is.null(title)) title <- sprintf('%s vs %s', ylab, xlab)
  plt.obj_gridstyle = invisible(plot_return_plot.obj(data = data, plt.obj = plt.obj))
  plt.obj_superimpose = invisible(plot_superimpose_advanced(data = data,
                                                            plt.obj = plt.obj,
                                                            ylab = ylab,
                                                            title = title,
                                                            legend_labels = legend_labels))
  if (class(plt.obj_gridstyle)[1] == "gg" && class(plt.obj_superimpose)[1] == "gg"){
    plt <- gridExtra::grid.arrange(plt.obj_gridstyle, plt.obj_superimpose, ncol = 2)
    print(plt)
    plt
  }
  else if (class(plt.obj_gridstyle)[1] == "plotly" && class(plt.obj_superimpose)[1] == "plotly"){
    plt <- plotly::subplot(plt.obj_gridstyle, plt.obj_superimpose, nrows = 1)
    print(plt)
    plt
  }
}

#' @rdname plotutils
#' @return ggplot list
#' @note The most useless function here but learnt plotly a little.
#' @keywords internal
plotly_plot <- function(plate, title = NULL, ylab = NULL, xlab = NULL) {
  data <- subset_or_not(plate)
  xvar <- get_x_var(data)
  yvar <- get_y_var(data)
  if(is.null(title)) title <- sprintf('%s vs %s',y_var(plate),x_var_two_label(plate))
  if(is.null(xlab)) xlab <- get_x_label(xvar)
  if(is.null(ylab)) ylab <- params(plate, 'GENERAL', 'Y_VAR')

  plt <- plotly::plot_ly(
    data,
    x = stats::as.formula(paste("~", xvar)),
    y = stats::as.formula(paste("~", yvar)),
                   type = 'scatter',
                   mode = 'lines+markers',
                   marker = list(opacity = 0.3, size = 5)
    ) %>%
      plotly::layout(
        title = title,
        xaxis = list(title = xlab),
        yaxis = list(title = ylab)
      )
  plt
}


# ------------------------------ Plot Util normfluodbf 1.5.2 --------------------------
#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_dev_deprecated <- function(df,
                                wells,
                                x = NULL,
                                well_colors = NULL,
                                xlim=NULL,
                                ylim=NULL,
                                xlab=NULL,
                                ylab=NULL,
                                title=NULL){

  if (is.null(x) && 'Time' %in% names(df) && 'Cycle_Number' %in% names(df)){
    x <- "Cycle_Number"
  }
  else if (!is.null(x)){
    x <- x
  }
  else {
    x <- "Cycle_Number"
  }

  #palette <- c("#0000FF", "#8B0000", "#006400", "purple", "orange", "pink", "cyan", "brown", "gray", "black")
  palette <- c("blue", "red", "green", "purple", "orange", "pink", "cyan", "brown", "gray", "black")
  if(is.null(well_colors))
    well_colors <- c(palette[1:3])
  else
    well_colors <- c(palette[1:length(palette)])

  if(is.null(xlab))
    xlab <- 'CYCLE NUMBER'

  if(is.null(ylab))
    ylab <- 'FLUORESCENCE'

  if(is.null(title))
    title <- 'FLUORESCENCE vs CYCLE_NUMBER'

  num_wells <- length(wells)
  if (num_wells > length(well_colors)) {
    stop("Number of wells exceeds the available color palette.")
  }

  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = x)) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(title = title, x = xlab, y = ylab)

  for (i in seq_along(wells)) {
    p <- p + ggplot2::geom_point(ggplot2::aes_string(y = wells[i]), color = well_colors[i], size = 3, alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes_string(y = wells[i]), color = well_colors[i], size = 0.8, alpha = 0.7)
  }

  legend_labels <- wells
  names(well_colors) <- legend_labels

  p <- p + ggplot2::scale_color_manual(name = 'wells', values = setNames(well_colors, wells))
  p <- plotly::ggplotly(p)
  return(p)
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_dev <- function(df,
                     wells,
                     x = NULL,
                     well_colors = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     title = NULL) {

  if (is.null(x) && 'Time' %in% names(df) && 'Cycle_Number' %in% names(df)) {
    x <- "Cycle_Number"
  } else if (!is.null(x)) {
    x <- x
  } else {
    x <- "Cycle_Number"
  }

  palette <- c("blue", "red", "green", "purple", "orange", "pink", "cyan", "brown", "gray", "black")
  if (is.null(well_colors))
    well_colors <- palette[1:length(wells)]

  if (is.null(xlab))
    xlab <- 'CYCLE NUMBER'

  if (is.null(ylab))
    ylab <- 'FLUORESCENCE'

  if (is.null(title))
    title <- 'FLUORESCENCE vs CYCLE NUMBER'

  num_wells <- length(wells)
  if (num_wells > length(well_colors)) {
    stop("Number of wells exceeds the available color palette.")
  }

  # Create a combined data frame for ggplot
  combined_df <- df %>%
    tidyr::pivot_longer(cols = tidyr::all_of(wells), names_to = "Well", values_to = "Fluorescence")

  p <- ggplot2::ggplot(combined_df, ggplot2::aes_string(x = x, y = "Fluorescence", color = "Well")) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_line(size = 0.8, alpha = 0.7) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(title = title, x = xlab, y = ylab, color = 'Well') +
    ggplot2::scale_color_manual(values = stats::setNames(well_colors, wells))

  p <- plotly::ggplotly(p)
  return(p)
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_dev_with_custom_legends <- function(df,
                                         wells,
                                         legend_labels = NULL,
                                         x = NULL,
                                         well_colors = NULL,
                                         xlim = NULL,
                                         ylim = NULL,
                                         xlab = NULL,
                                         ylab = NULL,
                                         title = NULL) {

  if (is.null(x) && 'Time' %in% names(df) && 'Cycle_Number' %in% names(df)) {
    x <- "Cycle_Number"
  } else if (!is.null(x)) {
    x <- x
  } else {
    x <- "Cycle_Number"
  }

  palette <- c("blue", "red", "green", "purple", "orange", "pink", "cyan", "brown", "gray", "black")
  if (is.null(well_colors)) {
    well_colors <- palette[1:length(wells)]
  }

  if (is.null(xlab)) {
    xlab <- 'CYCLE NUMBER'
  }

  if (is.null(ylab)) {
    ylab <- 'FLUORESCENCE'
  }

  if (is.null(title)) {
    title <- 'FLUORESCENCE vs CYCLE NUMBER'
  }

  num_wells <- length(wells)
  if (num_wells > length(well_colors)) {
    stop("Number of wells exceeds the available color palette.")
  }

  combined_df <- df %>%
    tidyr::pivot_longer(cols = tidyr::all_of(wells), names_to = "Well", values_to = "Fluorescence")

  if (is.null(legend_labels)) {
    legend_labels <- wells
  } else {
    if (length(legend_labels) != length(wells)) {
      stop("The length of legend_labels must match the length of wells.")
    }
  }

  combined_df$Legend <- factor(combined_df$Well, levels = wells, labels = legend_labels)

  p <- ggplot2::ggplot(combined_df, ggplot2::aes_string(x = x, y = "Fluorescence", color = "Legend")) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_line(size = 0.8, alpha = 0.7) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(title = title, x = xlab, y = ylab, color = 'Legend') +
    ggplot2::scale_color_manual(values = stats::setNames(well_colors, legend_labels))

  p <- plotly::ggplotly(p)
  return(p)
}

#----------------------------- Plot Handlers Continues ----------------------------- getNamespaceExports('ggplot2')

#' Multiplot
#' @param ... extra
#' @param plotlist list
#' @param file file
#' @param cols cols
#' @param layout layout
#' @return grid plot
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

# ---------------------------------- General Grids --------------------------

#' @rdname plotutils
#' @return ggplot list
#' @note Just simple grids with no functionality. No time to delve deeper but a good primer.
#' @keywords internal
plate_wells_grid <- function(num_wells=NULL){
  if (num_wells == 96) {
    gs <- lapply(1:96, function(ii)
      grid::grobTree(grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
    gridExtra::grid.arrange(grobs=gs, ncol=12,
                            top="top label", bottom="bottom\nlabel",
                            left="left label", right="right label")
    grid::grid.rect(gp=grid::gpar(fill=NA))
  }
  else if (num_wells == 384){
    gs <- lapply(1:384, function(ii)
      grid::grobTree(grid::rectGrob(gp=grid::gpar(fill=ii, alpha=0.5)), grid::textGrob(ii)))
    gridExtra::grid.arrange(grobs=gs, ncol=12,
                            top=c(LETTERS[1:8]),
                            left=c(1:12))
    grid::grid.rect(gp=grid::gpar(fill=NA))
  }
}
