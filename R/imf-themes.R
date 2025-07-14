#' Convert RGB Values to Color String
#'
#' Converts red, green, and blue values into a color string that can be used in plotting.
#' @param red Red component (0-255).
#' @param green Green component (0-255).
#' @param blue Blue component (0-255).
#' @return A color string.
#' @export
rgb2 <- function(red, green, blue) {
  col = rgb(red, green, blue, maxColorValue = 255)
  return(col)
}

## IMF Standard Colors
#' @export
imfblue <- rgb2(75, 130, 173)
#' @export
imfgreen <- rgb2(150, 186, 121)
#' @export
imfred <- rgb2(192, 0, 80)
#' @export
imfgrey <- rgb2(166, 168, 172)

# Additional IMF Colors
#' @export
imflight_blue <- rgb2(202, 224, 251)
#' @export
imflight_green <- rgb2(150, 215, 130)
#' @export
imflight_red <- rgb2(238, 36, 0)
#' @export
imflight_grey <- rgb2(211, 211, 211)
#' @export
imfdark_red <- rgb2(144, 0, 0)
#' @export
imfdark_blue <- rgb2(14, 16, 116)
#' @export
imfpurple <- rgb2(146, 60, 194)
#' @export
imforange <- rgb2(255, 133, 71)

#' List of IMF Colors
#'
#' Predefined color palette using standard IMF colors.
#' @export
A4_colors <- c(imfblue, imfgreen, imfdark_red, imfgrey, "black")

#' List of IMF Colors for Bar Charts
#'
#' Extended IMF color palette for bar charts, including lighter and darker variations.
#' @export
A4_colors_bar <- c(imfblue, imfgreen, imfdark_red, imfgrey, "black",
                   imflight_blue, imflight_green, imflight_red,
                   imflight_grey, imfdark_red, imfdark_blue)

#' Apply IMF Colors to ggplot2 helper function dynamically
#' @export
scale_imf_colors <- function(n = NULL) {
  available_colors <- A4_colors_bar  # Default to IMF Bar Colors

  # If the dataset has more categories than colors, recycle colors safely
  if (!is.null(n) && n > length(available_colors)) {
    warning("Not enough predefined IMF colors. Recycling colors.")
    available_colors <- rep(available_colors, length.out = n)
  }

  list(
    ggplot2::scale_color_manual(values = available_colors),
    ggplot2::scale_fill_manual(values = available_colors)
  )
}

# Ensure Segoe UI or a fallback font is available
windowsFonts(
  IMF = windowsFont("Segoe UI"),
  Fallback = windowsFont("Arial")  # Fallback to Arial if Segoe UI is missing
)

# Define Primary Font
primary_font <- if ("Segoe UI" %in% names(windowsFonts())) {
  "Segoe UI"
} else {
  "Arial"  # Default to Arial if Segoe UI isn't available
}

# ------------------------------------------------------------------------------
#' IMF-Themed ggplot2 Theme
#'
#' Applies an IMF-style ggplot2 theme with customized fonts, colors, and layout.
#' @param myfont Font family to use (default: Segoe UI or available alternative).
#' @param myfontColor Color for title and subtitle (default: `imfblue`).
#' @param use_imf_colors Logical. If true (Default), applies 'scale_imf_colors()'.
#' @param legend_title_format Formatting for the legend title (default: `element_blank()`).
#' @param legend_position Position of the legend (default: `"bottom"`).
#' @param legend_direction Direction of legend items (`"horizontal"` or `"vertical"`).
#' @param legend_key_height,legend_key_width,legend_spacing_x,legend_spacing_y Size and spacing of legend elements.
#' @return A ggplot2 theme.
#' @export
theme_imf <- function(myfont = primary_font, myfontColor = imfblue,
                      use_imf_colors = TRUE,
                      legend_title_format = ggplot2::element_blank(),
                      legend_position = "bottom", legend_direction = "horizontal",
                      legend_key_height = grid::unit(0.45, "cm"), legend_key_width = grid::unit(1.3, "cm"),
                      legend_spacing_x = grid::unit(0.2, "cm"), legend_spacing_y = grid::unit(0.25, "cm")) {

  # Ensure `grid` is loaded before using `unit()`
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required but not installed.")
  }

  ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 18, family = myfont),
      axis.text = ggplot2::element_text(color = "black", size = 18, family = myfont),
      axis.text.x = ggplot2::element_text(size = 18, family = myfont),
      axis.text.y = ggplot2::element_text(size = 18, family = myfont,
                                          margin = grid::unit(c(0, 3.5, 0, 0), "mm")),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.y.right = ggplot2::element_blank(),
      #axis.title.x = ggplot2::element_text(size = 18, family = myfont), #if wanted to add XY titles
      #axis.title.y = ggplot2::element_text(size = 18, family = myfont),
      axis.ticks.x = ggplot2::element_line(colour = imflight_grey),
      axis.ticks.y = ggplot2::element_line(colour = imflight_grey),
      axis.ticks.length.x = grid::unit(-2, "mm"),
      axis.ticks.length.y = grid::unit(-2, "mm"),
      plot.title = ggplot2::element_text(size = 24, colour = myfontColor, family = myfont, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 18, family = myfont, colour = myfontColor),
      panel.border = ggplot2::element_rect(colour = imflight_grey, fill = NA, size = 1),
      axis.line = ggplot2::element_line(colour = imflight_grey),
      plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
      legend.text = ggplot2::element_text(size = 12, family = myfont),
      legend.title = legend_title_format,
      legend.position = legend_position,
      legend.direction = legend_direction,
      legend.key.height = legend_key_height,
      legend.key.width = legend_key_width,
      legend.spacing.x = legend_spacing_x,
      legend.spacing.y = legend_spacing_y
    )
}

# ------------------------------------------------------------------------------
#' IMF-Themed Panel ggplot2 Theme
#'
#' Similar to `theme_imf()` but optimized for panel charts with smaller text sizes.
#' @inheritParams theme_imf
#' @return A ggplot2 theme for panel charts.
#' @export
theme_imf_panel <- function(myfont = primary_font, myfontColor = imfblue,
                            legend_title_format = ggplot2::element_blank(),
                            legend_position = "bottom", legend_direction = "horizontal",
                            legend_key_height = grid::unit(0.45, "cm"), legend_key_width = grid::unit(1.3, "cm"),
                            legend_spacing_x = grid::unit(0.2, "cm"), legend_spacing_y = grid::unit(0.25, "cm")) {

  # Ensure `grid` is available for unit()
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required but not installed.")
  }

  ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14, family = myfont),
      axis.text = ggplot2::element_text(color = "black", size = 10, family = myfont),
      axis.text.x = ggplot2::element_text(size = 10, family = myfont),
      axis.text.y = ggplot2::element_text(size = 10, family = myfont,
                                          margin = grid::unit(c(0, 3.5, 0, 0), "mm")),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.y.right = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = imflight_grey),
      axis.ticks.y = ggplot2::element_line(colour = imflight_grey),
      axis.ticks.length.x = grid::unit(-2, "mm"),
      axis.ticks.length.y = grid::unit(-2, "mm"),
      plot.title = ggplot2::element_text(size = 14, colour = myfontColor, family = myfont, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, family = myfont, colour = myfontColor, hjust = 0.5),
      axis.line = ggplot2::element_blank(),  # removes thick black axis lines
      panel.border = ggplot2::element_rect(colour = imflight_grey, fill = NA, size = 1),
      legend.text = ggplot2::element_text(size = 10, family = myfont),
      legend.title = legend_title_format,
      legend.position = legend_position,
      legend.direction = legend_direction,
      legend.key.height = legend_key_height,
      legend.key.width = legend_key_width,
      legend.spacing.x = legend_spacing_x,
      legend.spacing.y = legend_spacing_y
    )
}
