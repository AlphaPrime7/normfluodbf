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

# ------------------------------- axis labels ----------------

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
get_x_var = function(data){
  xvars = c()
  for (i in names(data)){
    if (i == "Cycle_Number"){
      xvars = c(xvars, i)
    }

    if (i == "Time"){
      xvars = c(xvars, i)
    }
  }
  return(xvars)
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

# ------------------------------- handle legends ----------------

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


# -------------------------------- Plot Handlers ----------------------------

## -------------------------------- base ----------------------------
#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
plot_canvas <- function(plate,
                        whichxvar = NULL,
                        Cycle_Number = 2 %in% whichxvar,
                        Time = 1 %in% whichxvar){
  bg_plot_app = "transparent"
  bg_plot_r = "#DDDDDD"

  xvariables = get_x_var(plate[['plate_data']])
  xlabel = NULL
  ylable = NULL

  #xlabel
  if(is.null(whichxvar)) {
    xvar = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2

    xlabel = x_var_two_label(plate)
  }
  else if (Cycle_Number && "Cycle_Number" %in% xvariables){
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  else if(Time && "Time" %in% xvariables) {
    whichxvar = 1
    xlabel = x_var_one_label(plate)
  }
  else {
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }

  #ylabel
  ylabel = y_var(plate)

  plt.obj <-
    ggplot2::ggplot() +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(sprintf('%s vs %s',ylabel,xlabel)) +
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

## -------------------------------- plot 1 ----------------------------

#' @rdname plotutils
#' @return ggplot list
#' @export
plot_grid <- function(data,
                      plt.obj){
  plt.obj <- plt.obj +
    ggplot2::facet_grid(row_levels_grid(data$well_row) ~ column_levels_grid(data$well_col))
  plt.obj
}

#' @rdname plotutils
#' @return ggplot list
#' @keywords internal
#' @note follows the grid plot
plot_in_well = function(plt.obj,
                        data,
                        whichxvar = NULL,
                        Cycle_Number = 2 %in% whichxvar,
                        Time = 1 %in% whichxvar,
                        no_annotations = FALSE){

  xvariables = get_x_var(data)
  x = NULL
  y = NULL

  #xlabel
  if(is.null(whichxvar)) {
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  else if (Cycle_Number && "Cycle_Number" %in% xvariables){
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  else if(Time && "Time" %in% xvariables) {
    x = xvariables[which(xvariables == "Time")]
    whichxvar = 1
    xlabel = x_var_one_label(plate)
  }
  else {
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }

  #ylabel
  ylabel = y_var(plate)
  y = get_y_var(data)

  #take in grid
  plt.obj <- plt.obj +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x, y),
      alpha = 0.3,
      size = 1
    ) +
    ggplot2::geom_line(data=data,
                       ggplot2::aes_string(x, y, group = 'well'),
                       linewidth=0.1)

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
#' @keywords internal
plot.fluorstar = function(data,
                          plt.obj,
                          whichxvar = NULL){
  plt.obj <- plot_grid(data = data,
                       plt.obj = plt.obj)
  plt.obj = plot_in_well(plt.obj,
                         data,
                         no_annotations = F,
                         whichxvar = whichxvar)
  print(plt.obj)
  plt.obj
}

## -------------------------------- plot 2 ----------------------------


#' @rdname plotutils
#' @return ggplot list
#' @note purely subordinate because of the plt.obj found in the plot function
#' @keywords internal
plot_superimpose <- function(data,
                             plt.obj,
                             whichxvar = NULL,
                             Cycle_Number = 2 %in% whichxvar,
                             Time = 1 %in% whichxvar,
                             xlab = NULL,
                             ylab = NULL,
                             title = NULL,
                             legend_labels = NULL,
                             whichpalette = NULL,
                             whichlinetype = NULL,
                             linetypeoptions = c("solid","dashed","dotted"),
                             solid = linetypeoptions[which(linetypeoptions == "solid")],
                             dashed = linetypeoptions[which(linetypeoptions == "dashed")],
                             dotted = linetypeoptions[which(linetypeoptions == "dotted")],
                             linetype = solid,
                             wespalettes = c(names(wesanderson::wes_palettes)),
                             Zissou1 = wespalettes[which(wespalettes == "Zissou1")],
                             Cavalcanti1 = wespalettes[which(wespalettes == "Cavalcanti1")],
                             BottleRocket1 = wespalettes[which(wespalettes == "BottleRocket1")],
                             BottleRocket2 = wespalettes[which(wespalettes == "BottleRocket2")],
                             Rushmore1 = wespalettes[which(wespalettes == "Rushmore1")],
                             Darjeeling1 = wespalettes[which(wespalettes == "Darjeeling1")],
                             Royal1 = wespalettes[which(wespalettes == "Royal1")],
                             Royal2 = wespalettes[which(wespalettes == "Royal2")],
                             Moonrise1 = wespalettes[which(wespalettes == "Moonrise1")],
                             GrandBudapest1 = wespalettes[which(wespalettes == "GrandBudapest1")],
                             Moonrise2 = wespalettes[which(wespalettes == "Moonrise2")],
                             GrandBudapest2 = wespalettes[which(wespalettes == "GrandBudapest2")],
                             IsleofDogs1 = wespalettes[which(wespalettes == "IsleofDogs1")],
                             IsleofDogs2 = wespalettes[which(wespalettes == "IsleofDogs2")],
                             FrenchDispatch = wespalettes[which(wespalettes == "FrenchDispatch")],
                             AsteroidCity1 = wespalettes[which(wespalettes == "AsteroidCity1")],
                             AsteroidCity2 = wespalettes[which(wespalettes == "AsteroidCity2")],
                             pal = Zissou1
                                      ) {

  #get x vars because of 2 options
  xvariables = get_x_var(data)
  x = NULL
  y = NULL

  if(!is.null(legend_labels))
    data <- add_custom_legend(data,
                              legend_labels)
  else
    data <- data

  #not consistent with plot 1 (most beloved plot)
  used_wells = data[!is.na(data[['fluor_values']]),] #dont want a plot of everything if possible
  sample_cols <- unique(data[['well']]) #legend should only contain used wells

  #xlabel and xvar
  if(is.null(whichxvar)) {
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  else if (Cycle_Number && "Cycle_Number" %in% xvariables){
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  else if(Time && "Time" %in% xvariables) {
    x = xvariables[which(xvariables == "Time")]
    whichxvar = 1
    xlabel = x_var_one_label(plate)
  }
  else {
    x = xvariables[which(xvariables == "Cycle_Number")]
    whichxvar = 2
    xlabel = x_var_two_label(plate)
  }
  if (is.null(xlab)) xlab = xlabel

  #ylabel and yvar
  ylabel = y_var(plate)
  y = get_y_var(data)
  if (is.null(ylab)) ylab = ylabel

  #grouping variables
  well = 'well'
  legend = 'legend'

  #title(see if the canvas does not get this)
  if (is.null(title)) title <- sprintf('%s vs %s', ylabel, xlabel)

  #palette management -if is the turn on
  if(is.null(whichpalette)) {
    whichpalette = 1
  }
  pal = switch(whichpalette,
               Zissou1,
               Cavalcanti1,
               BottleRocket1,
               BottleRocket2,
               Rushmore1,
               Darjeeling1,
               Royal1,
               Royal2,
               Moonrise1,
               GrandBudapest1,
               Moonrise2,
               GrandBudapest2,
               IsleofDogs1,
               IsleofDogs2,
               FrenchDispatch,
               AsteroidCity1,
               AsteroidCity2,
               Zissou1
               )

  #linetype management
  if(is.null(whichlinetype)) whichlinetype = 1

  linetype = switch(whichlinetype,
               solid,
               dashed,
               dotted,
               solid
               )

  if("legend" %in% colnames(used_wells)){
    plt.obj <- plt.obj + ggplot2::geom_point(
      data = used_wells,
      ggplot2::aes_string(x = x,
                          y = y,
                          color = legend),
      size = 2,
      shape = 16,
      show.legend = TRUE
    ) +
      ggplot2::geom_line(
        data = used_wells,
        ggplot2::aes_string(x = x,
                            y = y,
                            group = well,
                            color = legend),
        size = 0.8,
        linetype = linetype,
        alpha = 0.7
      ) +
      ggplot2::labs(color = legend) +
      ggplot2::theme(legend.position = "right")
  }
  else {
    plt.obj <- plt.obj + ggplot2::geom_point(
      data = used_wells,
      ggplot2::aes_string(x = x,
                          y = y,
                          color = well),
      size = 1,
      show.legend = TRUE
    ) +
      ggplot2::geom_line(
        data = used_wells,
        ggplot2::aes_string(x = x,
                            y = y,
                            group = well,
                            color = well),
        size = 0.8,
        alpha = 0.7
      ) +
      ggplot2::labs(color = well)
  }

  if ("legend" %in% colnames(used_wells)) {
    legend_labels <- unique(used_wells$legend)
    plt.obj <- plt.obj + ggplot2::scale_color_manual(
      name = well,
      values = wesanderson::wes_palette(n = length(legend_labels),
                                        name = pal,
                                        type = 'continuous'),
      breaks = legend_labels,
      labels = legend_labels
    )
  }
  else {
    plt.obj <- plt.obj + ggplot2::scale_color_manual(
      name = well,
      values = wesanderson::wes_palette(n = length(sample_cols),
                                        name = pal,
                                        type = 'continuous'),
      breaks = sample_cols
    )
  }

  print(plt.obj)
  return(plt.obj)
}

## -------------------------------- plot 3 ----------------------------

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
  #plt.obj <- plotly::ggplotly(plt.obj)
  print(plt.obj)
  return(plt.obj)
}


## -------------------------------- plot 4 ----------------------------

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
                              whichxvar = NULL,
                              whichpalette = NULL,
                              whichlinetype = NULL,
                              legend_labels = NULL){



  if (is.null(title)) title <- sprintf('%s vs %s', ylab, xlab)
  plt.obj_gridstyle = invisible(plot.fluorstar(data = data,
                                                     plt.obj = plt.obj,
                                                     whichxvar = whichxvar))
  plt.obj_superimpose = invisible(plot_superimpose(data = data,
                                                   plt.obj = plt.obj,
                                                   legend_labels = legend_labels,
                                                   whichxvar = whichxvar,
                                                   whichpalette = whichpalette,
                                                   whichlinetype = whichlinetype))

  suppressWarnings({
  plt <- gridExtra::grid.arrange(plt.obj_gridstyle, plt.obj_superimpose, ncol = 2)
  })
  print(plt)
  plt

}

# -------------------------------- save plot ----------------------------

#' @rdname plotutils
#' @return plot utils
#' @keywords internal
#' @export
save_plot <- function(name){
  ggplot2::ggsave(name, width=9,height=6)
}
