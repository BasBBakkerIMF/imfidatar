# Ensure Segoe UI is recognized by windowsFonts
windowsFonts(`Segoe UI` = windowsFont("Segoe UI"))

# Define Primary Font
primary_font <- if ("Segoe UI" %in% names(windowsFonts())) {
  "Segoe UI"
} else {
  "Arial"  # Fallback if Segoe UI isn't installed
}

# Define pipe operator
usethis::use_pipe()

# ------------------------------------------------------------------------------
# Custom geom_line() function class
# which inherits from ggplot2::GeomLine
GeomLine2 <- ggplot2::ggproto("GeomLine2", ggplot2::GeomLine,
                              draw_key =  function (data, params, size) {
                                data$linetype[is.na(data$linetype)] <- 0
                                # Modified x0 / x1 values to shift
                                # the start / end points closer to the center in order
                                # to leave more space for the rounded ends
                                grid::segmentsGrob(0.2, 0.5, 0.8, 0.5,
                                                   gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                                                   wd = data$size * .pt,
                                                                   lty = data$linetype,
                                                                   lineend = "round"),
                                                   arrow = params$arrow)
                              }
)

# ------------------------------------------------------------------------------
# Function that uses GeomLine2 as the geom to create line chart.
geom_line2 <- function (mapping = NULL, data = NULL,
                        stat = "identity",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = stat,
                 # only change from geom_line to geom_line2
                 geom = GeomLine2,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...)
  )
}

# ------------------------------------------------------------------------------
# New ggline_core function that uses geom_line2
# ggpubr:::ggline_core
ggline_core_mod <- function (data, x, y,
                             group = 1,
                             numeric.x.axis = FALSE,
                             color = "black",
                             fill = "white",
                             palette = NULL,
                             linetype = "solid",
                             plot_type = c("b", "l", "p"),
                             size = 0.5,
                             shape = 19,
                             stroke = NULL,
                             point.size = size,
                             point.color = color,
                             title = NULL, xlab = NULL, ylab = NULL,
                             select = NULL, order = NULL,
                             facet.by = NULL,
                             add = "none",
                             add.params = list(),
                             error.plot = "errorbar",
                             show.line.label = FALSE,
                             font.label = list(size = 11, color = "black"),
                             repel = FALSE,
                             label.rectangle = FALSE,
                             position = "identity",
                             ggtheme = ggpubr::theme_pubr(), ...){

  xx <- ggpubr:::.select_vec(data, x)
  if (inherits(xx, c("character", "numeric")) & !numeric.x.axis){
    data[, x] <- ggpubr:::.select_vec(data, x) %>% as.factor()
  }

  error.plot = error.plot[1]
  plot_type <- match.arg(plot_type)
  if ("none" %in% add)
    add <- "none"
  grouping.vars <- intersect(c(x, color, linetype, group, facet.by),
                             names(data))
  . <- NULL
  if (is.null(add.params$fill)){
    add.params$fill <- "white"
  }

  add.params <- ggpubr:::.check_add.params(add, add.params, error.plot,
                                           data, color, fill = "white", ...)

  if (any(ggpubr:::.summary_functions() %in% add)){
    data_sum <- ggpubr:::desc_statby(data, measure.var = y, grps = grouping.vars)
    summary.funcs <- intersect(ggpubr:::.summary_functions(), add)
    if (length(summary.funcs) > 1)
      stop("Only one summary function is allowed. ",
           "Choose one of ", ggpubr:::.collapse(ggpubr:::.summary_functions(),
                                                sep = ", "))
    .center <- summary.funcs %>% strsplit("_", fixed = TRUE) %>%
      unlist() %>% .[1]
    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
    if (inherits(xx, c("character", "numeric")) &
        !numeric.x.axis)
      data_sum[, x] <- ggpubr:::.select_vec(data_sum, x) %>% as.factor()
  } else{
    data_sum <- data
  }

  .cols <- unique(c(color, linetype, group))
  if (any(.cols %in% names(data))) {
    .in <- which(.cols %in% names(data))
    group <- .cols[.in]
    if (is.null(add.params$group)){
      add.params$group <- group[1]
    }
  }

  # Create ggplot
  p <- ggplot2::ggplot(data, ggpubr::create_aes(list(x = x, y = y)))
  add.params <- add.params %>%
    # add geom_line2
    ggpubr:::.add_item(error.plot = error.plot,
                       position = position, p_geom = "geom_line2")

  p <- add.params %>%
    ggpubr:::.add_item(p = p,
                       add = setdiff(add, ggpubr:::.summary_functions())
    ) %>%
    do.call(ggpubr::ggadd, .)

  p <- add.params %>%
    ggpubr:::.add_item(p = p, size = size,
                       add = intersect(add, ggpubr:::.summary_functions())) %>%
    do.call(ggpubr::ggadd, .)

  if (plot_type %in% c("b", "l")) {
    line_args <- ggpubr::geom_exec(NULL, data = data_sum, stat = "identity",
                                   color = color, linetype = linetype,
                                   position = position, size = size)

    mapping <- line_args$mapping
    mapping[["group"]] <- group
    option <- line_args$option
    option[["mapping"]] <- ggpubr::create_aes(mapping)
    p <- p + do.call(geom_line2, option)
  }

  if (plot_type %in% c("p", "b")) {
    p <- p + ggpubr::geom_exec(ggplot2::geom_point, data = data_sum,
                               color = point.color,
                               shape = shape,
                               size = 1.2 + point.size,
                               stroke = stroke,
                               position = position)
    p <- ggpubr:::.scale_point_shape(p, data_sum, shape)
  }

  user.add.color <- list(...)$user.add.color
  if (is.null(user.add.color))
    user.add.color <- ""

  if (ggpubr:::.is_color(user.add.color) & !is.numeric(group)) {
    ngroup <- nlevels(ggpubr:::.select_vec(data_sum, group))
    palette <- rep(user.add.color, ngroup)
  }

  if (show.line.label & !is.numeric(group)) {
    xval <- ggpubr:::.select_vec(data_sum, x)
    last.xval <- ggpubr:::.levels(xval) %>% utils::tail(1)
    groupval <- ggpubr:::.select_vec(data_sum, group)
    label.data <- subset(data_sum, xval == last.xval)
    font.label <- ggpubr:::.parse_font(font.label)
    p <- font.label %>% ggpubr:::.add_item(data = label.data,
                                           x = x, y = y,
                                           label = group, repel = repel,
                                           label.rectangle = label.rectangle,
                                           ggtheme = ggtheme,
                                           ggp = p) %>%
      do.call(ggtext, .)
  }

  p <- ggpubr::ggpar(p, palette = palette, ggtheme = ggtheme, title = title,
                     xlab = xlab, ylab = ylab, ...)
  p
}

# ------------------------------------------------------------------------------
# New ggline function that uses ggline_core_mod, which includes geom_line2
ggline_mod <- function (data, x, y, group = 1, numeric.x.axis = FALSE, combine = FALSE,
                        merge = FALSE, color = "black", palette = NULL, linetype = "solid",
                        plot_type = c("b", "l", "p"), size = 0.5,
                        shape = 19, stroke = NULL, point.size = size, point.color = color,
                        title = NULL, xlab = NULL, ylab = NULL, facet.by = NULL,
                        panel.labs = NULL, short.panel.labs = TRUE, select = NULL,
                        remove = NULL, order = NULL, add = "none", add.params = list(),
                        error.plot = "errorbar", label = NULL, font.label = list(size = 11,
                                                                                 color = "black"), label.select = NULL, repel = FALSE,
                        label.rectangle = FALSE, show.line.label = FALSE, position = "identity",
                        ggtheme = ggpubr::theme_pubr(), ...)
{
  .opts <- list(group = group, numeric.x.axis = numeric.x.axis,
                combine = combine, merge = merge, color = color, palette = palette,
                linetype = linetype, plot_type = plot_type, size = size,
                shape = shape, stroke = stroke, point.size = point.size,
                point.color = point.color, title = title, xlab = xlab,
                ylab = ylab, facet.by = facet.by, panel.labs = panel.labs,
                short.panel.labs = short.panel.labs, select = select,
                remove = remove, order = order, add = add, add.params = add.params,
                error.plot = error.plot, label = label, font.label = font.label,
                label.select = label.select, repel = repel, label.rectangle = label.rectangle,
                show.line.label = show.line.label, position = position,
                ggtheme = ggtheme, ...)
  if (!missing(data))
    .opts$data <- data
  if (!missing(x))
    .opts$x <- x
  if (!missing(y))
    .opts$y <- y
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL
  for (opt.name in names(.opts)) {
    if (is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }
  .opts$fun <- ggline_core_mod
  .opts$fun_name <- "ggline"
  if (missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- ggpubr::theme_pubr(border = TRUE)
  p <- do.call(ggpubr:::.plotter, .opts)
  if (ggpubr:::.is_list(p) & length(p) == 1)
    p <- p[[1]]
  return(p)
}

# ------------------------------------------------------------------------------
#' Function to create a line chart with IMF stylings
#'
#' Create a ggplot2 line chart with IMF stylings. Takes in at minimum a
#' data.frame or similar and column names for x and y.
#' See also [imf.bookr::plot_bar_chart()] and [imf.bookr::plot_scatter_chart()].
#' Wrapper function for [ggpubr::ggline()], which is a wrapper for [pggplot2].
#'
#' @param input_data Input data such as a data.frame
#' @param var_x,var_y Column names in `input_data` for x-axis and y-axis, respectively
#' @param line_color Line and point color, default is imfblue
#' @param col_palette Color palette to use when multiple lines are drawn.
#'   Defaults to [A4_colors].
#' @param line_size Width of lines, defaults to 2.5
#' @param line_type Line type, defaults to "solid", can choose from
#'   "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash".
#' @param plot_type Specify type of chart, default to "l" for line, can also
#'   specify "p" for markers, and "b" for both lines and markers.
#' @param point_color Color of markers if `plot_type` is "p" or "b", defaults to `line_color`.
#' @param point_size Color of markers, defaults to `line_size`.
#' @param point_shape Shape of markers, defaults to 19 (solid circle).
#' @param point_label Labels on points.
#' @param point_repel Whether to repel labels on points.
#' @param font_label  Options for fonts of labesl, defaults to c(7, imfgrey).
#' @param font_family Font family to use, defaults to Segoe UI if available.
#' @param x_label X-axis label, defaults to empty string.
#' @param y_label Y-axis label, defaults to empty string.
#' @param plot_title Chart title, defaults to empty string.
#' @param plot_subtitle Subtitle for chart, defaults to empty string.
#' @param plot_caption Caption for chart, defaults to empty string.
#' @param panel Whether the output is a panel, affects style that is applied.
#' @param legend_title Legend title, defaults to empty string
#' @param legend_title_format Legend title format, defaults to `ggplot2::element_blank()`
#' @param legend_position, Legend position, defaults to `"bottom",`
#' @param legend_direction, Legend direction, defaults to `"horizontal",`
#' @param legend_key_height, Legend key height, defaults to `unit(0.45, "cm"),`
#' @param legend_key_width, Legend key width, defaults to `unit(1.3, "cm"),`
#' @param legend_spacing_x, Legend spacing for x direction, defaults to `unit(0.2, "cm"),`
#' @param legend_spacing_y, Legend spacing for y direction, defaults to `unit(0.25, "cm"),`
#' @param plotname If specified, the output name of the chart. Default is `NA`
#'   in which case chart is created but not saved as file.
#' @param charttype Specify from "single" (default), "full_panel","half_panel".
#'   Determines the size of the output file if `plotname` is provided.
#' @param dpi Resolution to use if `plotname` is specified and applicable. Defaults to `600`
#'


#'
#' @seealso [plot_bar_chart()], [plot_scatter_chart()]
#' @export
plot_line_chart <- function(input_data,
                            var_x,
                            var_y,
                            line_color = imfblue,
                            col_palette = A4_colors,
                            line_size = 1.0 ,
                            line_type = "solid",
                            plot_type = "l",
                            point_color = line_color,
                            point_size = line_size,
                            point_shape = 19,
                            point_label = NULL,
                            point_repel = NULL,
                            font_label = c(7, imfgrey),
                            font_family = primary_font,
                            x_label = "",
                            y_label = "",
                            plot_title = "",
                            plot_subtitle = "",
                            plot_caption = NULL,
                            panel = FALSE,
                            width = 9.46,
                            height = 6.85,
                            # Legend options
                            legend_title="",
                            legend_title_format=ggplot2::element_blank(),
                            legend_position="bottom",
                            legend_direction="horizontal",
                            legend_key_height=unit(0.45, "cm"),
                            legend_key_width=unit(1.3, "cm"),
                            legend_spacing_x=unit(0.2, "cm"),
                            legend_spacing_y=unit(0.25, "cm"),
                            # Save options
                            # If plotname is given, save
                            plotname = NA,
                            # Select charttype from
                            # "single","full_panel","half_panel"
                            charttype="single",
                            dpi=600)
{

  stopifnot(!missing(input_data))
  stopifnot(!missing(var_x))
  stopifnot(!missing(var_y))
  stopifnot(is.character(var_x))
  stopifnot(is.character(var_y))

  num_x_check <- is.numeric(input_data[[var_x]])

  ggplot_plot <-
    ggline_mod(data = input_data,
               x = var_x,
               y = var_y,
               plot_type = plot_type,
               palette = col_palette,
               color = line_color,
               size = line_size,
               linetype = line_type,
               shape = point_shape,
               point.size = point_size,
               point.color = point_color,
               # Labeling points on lines
               label = point_label,
               font.label = font_label,
               font.family = font_family,
               label.select = NULL,
               repel = point_repel,
               # Chart text
               title = plot_title,
               subtitle = plot_subtitle,
               xlab = x_label, ylab = y_label,
               caption = plot_caption,
               numeric.x.axis =  ifelse(isTRUE(num_x_check), TRUE, FALSE))

  if(isTRUE(panel)){
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf_panel(legend_title_format=legend_title_format,
                      legend_position=legend_position,
                      legend_direction=legend_direction,
                      legend_key_height=legend_key_height,
                      legend_key_width=legend_key_width,
                      legend_spacing_x=legend_spacing_x,
                      legend_spacing_y=legend_spacing_y
      )

  }else{
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf(legend_title_format=legend_title_format,
                legend_position=legend_position,
                legend_direction=legend_direction,
                legend_key_height=legend_key_height,
                legend_key_width=legend_key_width,
                legend_spacing_x=legend_spacing_x,
                legend_spacing_y=legend_spacing_y)
  }

  # Add legend title if not ""
  if (legend_title != ""){
    ggplot_plot <- ggplot_plot + ggplot2::guides(fill=ggplot2::guide_legend(title=legend_title))
  }

  # Save
  if (!is.na(plotname) && !panel) {
    save_plot(ggplot_plot_with_theme, plotname, type=charttype, dpi=dpi, width=width, height=height)
  }


  return(ggplot_plot_with_theme)
  rm(ggplot_plot, ggplot_plot_with_theme)

}

# ------------------------------------------------------------------------------
#' Function to create a bar chart with IMF stylings
#'
#' Creates a [ggplot2] bar chart with IMF stylings. Takes in at minimum a
#' data.frame or similar and column names for x and y.
#' See also [bookr::plot_line_chart()] and [bookr::plot_scatter_chart()].
#' Wrapper function for [ggpubr::ggbarplot()], which is a wrapper for [ggplot2].
#'
#' @inheritParams plot_line_chart
#' @param fill_var Color to fill bars, defaults to `imfblue`
#' @param fill_colors Colors to use for fill, defaults to `A4_colors_bar`
#' @param bar_order Order of bars, defaults to `NULL`
#' @param bar_sort Sorting order of bars, defaults to `c("none", "desc", "asc")`
#' @param bar_group_sort Whether to group sort, defaults to `FALSE`
#' @param bar_position Position of bars, defaults to `ggplot2::position_stack()`
#'   can also use ggplot2::position_dodge() to show side-by-side
#' @param bar_outline_color Outline color of bars, defaults to `"black"``
#' @param bar_width Width of bars, defaults to `0.5`
#' @param bar_label Whether to label bars with a given column, defaults to `FALSE`
#' @param bar_label_rounding Rounding of labels, defaults to `1`
#' @param bar_label_position Label position, defaults to `c("out", "in")`
#' @param bar_label_adjustment Adjustment of labels, defaults to `c(NULL, NULL)`
#' @param bar_label_color Label color of bars, defaults to `"black"`
#' @param bar_label_size Label size of bars, defaults to `4`
#' @param plot_caption Caption for chart, defaults to empty string.
#'

#'
#'
#' @seealso [plot_line_chart()], [plot_scatter_chart()]

#' @export
plot_bar_chart <- function(input_data,
                           var_x,
                           var_y,
                           fill_var = imfblue,
                           fill_colors = A4_colors_bar,
                           bar_order = NULL,
                           bar_sort =  c("none", "desc", "asc"),
                           bar_group_sort = FALSE,
                           bar_position = ggplot2::position_stack(),
                           bar_outline_color = "black",
                           bar_outline_size = 0.25,
                           bar_width = 0.5 ,
                           bar_label = FALSE,
                           bar_label_rounding = 1,
                           bar_label_position = c("out", "in"),
                           bar_label_adjustment = c(NULL, NULL),
                           bar_label_color= "black",
                           bar_label_size = 4,
                           x_label = "",
                           y_label = "",
                           plot_title = "",
                           plot_subtitle = "",
                           plot_caption = NULL,
                           panel = FALSE,
                           width = 9.46,
                           height = 6.85,
                           # Legend options
                           legend_title="",
                           legend_title_format=ggplot2::element_blank(),
                           legend_position="bottom",
                           legend_direction="horizontal",
                           legend_key_height=unit(0.45, "cm"),
                           legend_key_width=unit(1.3, "cm"),
                           legend_spacing_x=unit(0.2, "cm"),
                           legend_spacing_y=unit(0.25, "cm"),
                           # Save options
                           # If plotname is given, save
                           plotname = NA,
                           # Select charttype from
                           # "single","full_panel","half_panel"
                           charttype="single",
                           dpi=600){

  stopifnot(!missing(input_data))
  stopifnot(!missing(var_x))
  stopifnot(!missing(var_y))
  stopifnot(is.character(var_x))
  stopifnot(is.character(var_y))

  if (is.null(plot_caption) || trimws(plot_caption) == "") {
    plot_caption <- NULL
  }

  ggplot_plot <-
    ggpubr::ggbarplot(data = input_data,
                      x = var_x, y = var_y,
                      # Outline color of bar
                      color = bar_outline_color,
                      size = bar_outline_size,
                      fill = fill_var,
                      # Fill color palette
                      palette = fill_colors,
                      # Order and Sorting of Bars
                      order = bar_order,
                      sort.val = bar_sort,
                      sort.by.groups = bar_group_sort,
                      # Bar stacked or not
                      position = bar_position,
                      # Bar and point size parameters
                      # Bar fill
                      width = bar_width,
                      # Labeling points
                      label = bar_label,
                      lab.col = bar_label_color,
                      lab.size = bar_label_size,
                      lab.pos = bar_label_position,
                      lab.vjust = bar_label_adjustment[1],
                      lab.hjust = bar_label_adjustment[2],
                      lab.nb.digits = bar_label_rounding,
                      # Chart text
                      title = plot_title,
                      subtitle = plot_subtitle,
                      xlab = x_label, ylab = y_label,
                      caption = plot_caption
    )

  # Add legend title if not ""
  if (legend_title != ""){
    ggplot_plot <- ggplot_plot + ggplot2::guides(fill=ggplot2::guide_legend(title=legend_title))
  }

  if(isTRUE(panel)){
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf_panel(legend_title_format=legend_title_format,
                      legend_position=legend_position,
                      legend_direction=legend_direction,
                      legend_key_height=legend_key_height,
                      legend_key_width=legend_key_width,
                      legend_spacing_x=legend_spacing_x,
                      legend_spacing_y=legend_spacing_y)
  }else{
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf(legend_title_format=legend_title_format,
                legend_position=legend_position,
                legend_direction=legend_direction,
                legend_key_height=legend_key_height,
                legend_key_width=legend_key_width,
                legend_spacing_x=legend_spacing_x,
                legend_spacing_y=legend_spacing_y)
  }

  # patchwork_plot <- ggplot_plot + plot_annotation()
  # plot(ggplot_plot)

  # Save
  if (!is.na(plotname) && !panel) {
    save_plot(ggplot_plot_with_theme, plotname, type=charttype, dpi=dpi, width=width, height=height)
  }

  return(ggplot_plot_with_theme)
  rm(ggplot_plot, ggplot_plot_with_theme)

}
# ------------------------------------------------------------------------------

#' Function to create a scatter chart with IMF stylings
#'
#' Creates a [ggplot2] scatter chart with IMF stylings. Takes in at minimum a
#' data.frame or similar and column names for x and y.
#' See also [imf.bookr::plot_line_chart()] and [imf.bookr::plot_bar_chart()].
#' Wrapper function for [ggpubr::ggscatter()], which is a wrapper for [ggplot2].
#'
#' @inheritParams plot_line_chart
#' @param input_data Input data such as a data.frame
#' @param var_x,var_y Column names in `input_data` for x-axis and y-axis, respectively
#' @param reg_line Whether to show a regression line, defaults to `TRUE`
#' @param reg_line_color Color of regression line, defaults to `imfgrey`
#' @param fill_var If a color is given, use that color to fill markers. If a column name is
#'   given, use that column's values to change marker colors.
#' @param fill_colors Colors to use to fill markers, defaults to `A4_colors`
#' @param point_outline_color Outline color of markers, defaults to "black"
#' @param point_size Marker size, defaults to 2.4
#' @param point_shape Marker shae, defaults to 21
#' @param point_label Whether to label points, set column name if necessary, defaults to `NULL
#' @param label_repel Whether to repel labels, defaults to `TRUE`
#' @param font_label Font parameters, defaults to `c(7, imfgrey)`
#' @param font_family Font family, defaults to `primary_font` which should be Segoe UI
#' @param plot_caption Caption for chart, defaults to empty string.
#'
#'
#' @seealso [plot_line_chart()], [plot_bar_chart()]

#' @export
plot_scatter_chart <- function(input_data,
                               var_x,
                               var_y,
                               reg_line = TRUE,
                               reg_line_color = imfgrey,
                               point_color = imfblue,
                               point_outline_color = "black",
                               point_size = 2.4,
                               point_shape = 21,
                               point_label = NULL,
                               label_repel = TRUE,
                               font_label = c(7, imfgrey),
                               font_family = primary_font,
                               x_label = "",
                               y_label = "",
                               plot_title = "",
                               plot_subtitle = NULL,
                               plot_caption = NULL,
                               panel = FALSE,
                               width = 9.46,
                               height = 6.85,
                               legend_title = "",
                               legend_title_format = ggplot2::element_blank(),
                               legend_position = "bottom",
                               legend_direction = "horizontal",
                               legend_key_height = unit(0.45, "cm"),
                               legend_key_width = unit(1.3, "cm"),
                               legend_spacing_x = unit(0.2, "cm"),
                               legend_spacing_y = unit(0.25, "cm"),
                               plotname = NA,
                               charttype = "single",
                               dpi = 600){

  stopifnot(!missing(input_data))
  stopifnot(!missing(var_x))
  stopifnot(!missing(var_y))
  stopifnot(is.character(var_x))
  stopifnot(is.character(var_y))

  if (is.null(plot_caption) || trimws(plot_caption) == "") {
    plot_caption <- NULL
  }


  ggplot_plot <- ggpubr::ggscatter(
    data = input_data,
    x = var_x,
    y = var_y,
    add = ifelse(isTRUE(reg_line), "reg.line", "none"),
    add.params = list(color = reg_line_color, fill = reg_line_color),
    fill = point_color,                  # this is the interior of the points
    color = point_outline_color,        # this is the border
    size = point_size,
    shape = point_shape,
    label = point_label,
    font.label = font_label,
    font.family = font_family,
    label.select = NULL,
    repel = label_repel,
    label.rectangle = FALSE,
    title = plot_title,
    subtitle = plot_subtitle,
    xlab = x_label,
    ylab = y_label,
    caption = plot_caption
  )

  # Add legend title if not ""
  if (legend_title != ""){
    ggplot_plot <- ggplot_plot + ggplot2::guides(fill=ggplot2::guide_legend(title=legend_title))
  }

  if(isTRUE(panel)){
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf_panel(legend_title_format=legend_title_format,
                      legend_position=legend_position,
                      legend_direction=legend_direction,
                      legend_key_height=legend_key_height,
                      legend_key_width=legend_key_width,
                      legend_spacing_x=legend_spacing_x,
                      legend_spacing_y=legend_spacing_y) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
                     axis.ticks.x = ggplot2::element_line(colour = imflight_grey),
                     axis.ticks.length.x = ggplot2::unit(-2, "mm"),
                     plot.caption = ggplot2::element_text(hjust = 0,vjust = 1),
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), angle = 90)
                     )
  }else{
    ggplot_plot_with_theme <-
      ggplot_plot +
      theme_imf(legend_title_format=legend_title_format,
                legend_position=legend_position,
                legend_direction=legend_direction,
                legend_key_height=legend_key_height,
                legend_key_width=legend_key_width,
                legend_spacing_x=legend_spacing_x,
                legend_spacing_y=legend_spacing_y) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm")),
                     axis.ticks.x = ggplot2::element_line(colour = imflight_grey),
                     axis.ticks.length.x = ggplot2::unit(-2, "mm"),
                     plot.caption = ggplot2::element_text(hjust = 0,vjust = 1),
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), angle = 90)
      )
  }

  # patchwork_plot <- ggplot_plot + plot_annotation()
  # plot(ggplot_plot)

  ## Save
  if (!is.na(plotname) && !panel) {
    save_plot(ggplot_plot_with_theme, plotname, type=charttype, dpi=dpi, width=width, height=height)
  }

  return(ggplot_plot_with_theme)
  rm(ggplot_plot, ggplot_plot_with_theme)

}

# ------------------------------------------------------------------------------
# (Your other functions such as plot_line_chart(), plot_bar_chart(), etc.)

# ------------------------------------------------------------------------------
#' Save a ggplot with default IMF dimensions
#'
#' This function wraps `ggsave()` to automatically apply IMF default width, height, and DPI.
#'
#' @param plot A ggplot2 object
#' @param filename The filename (e.g., "plot.png")
#' @param dpi Resolution (default: IMF standard 600)
#' @param width Width in inches (default: IMF standard 9.46)
#' @param height Height in inches (default: IMF standard 6.85)
#' @export
ggsave_imf <- function(plot, filename, width = NULL, height = NULL, dpi = NULL) {
  # Retrieve IMF defaults if width, height, or dpi are not specified
  width <- ifelse(is.null(width), getOption("imf_plot_width", 9.46), width)
  height <- ifelse(is.null(height), getOption("imf_plot_height", 6.85), height)
  dpi <- ifelse(is.null(dpi), getOption("imf_dpi", 600), dpi)

  # Save using ggsave with IMF defaults
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi, units = "in")
}

# ------------------------------------------------------------------------------
#' Save plot with predefined settings
#'
#' Uses `ggsave_imf()` to apply IMF default width, height, and DPI settings.
#'
#' @param plot The ggplot2 object to be saved.
#' @param filename The file name for the saved plot.
#' @param type Chart type, defaults to "single".
#' @param dpi Dots per inch for output quality (default: IMF standard 600).
#' @param width Plot width in inches (default: IMF standard 9.46).
#' @param height Plot height in inches (default: IMF standard 6.85).
#' @export
save_plot <- function(plot, filename, type = "single", dpi = NULL, width = NULL, height = NULL) {
  ggsave_imf(plot, filename, dpi = dpi, width = width, height = height)
}


# ------------------------------------------------------------------------------
#' Plot Dual Axis Time Series Chart with IMF Styling
#'
#' Plots two time series on the same x-axis using dual y-axes.
#' The left series appears in steelblue, and the right series is scaled and shown in firebrick.
#'
#' @param data A data.frame or tibble with a `date` column and two numeric columns.
#' @param var_left Unquoted name of the column for the left y-axis.
#' @param var_right Unquoted name of the column for the right y-axis.
#' @param plot_title Chart title.
#' @param y_left_lbl Left y-axis label.
#' @param y_right_lbl Right y-axis label.
#' @param plot_subtitle Optional subtitle for the chart.
#' @param caption Optional caption for the chart.
#' @param panel Logical, whether to apply `theme_imf_panel()` (TRUE) or `theme_imf()` (FALSE).
#' @param plotname Optional filename (e.g., "plot.png") to save the plot.
#' @param charttype One of "single", "full_panel", or "half_panel" to set dimensions if saving.
#' @param dpi Image resolution in dots per inch. Default is 600.
#' @param width Plot width in inches (default 9.46).
#' @param height Plot height in inches (default 6.85).
#'
#' @return A ggplot2 object with IMF-styled dual y-axes.
#' @export
#'
