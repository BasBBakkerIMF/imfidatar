#' @importFrom imf.bookr imfdata_by_key imfdata_by_countries_and_series
#' @importFrom imf.bookr imfdata_show_datasets get_dimension_names get_dimension_env
#' @importFrom imf.bookr make_key_str
NULL

# This function runs automatically when the package is attached (library(imf.bookr))
.onAttach <- function(libname, pkgname) {
  # Ensure grid is available for unit()
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required but not installed.")
  }

  # ✅ Set IMF Theme Globally
  ggplot2::theme_set(imf.bookr::theme_imf())

  # ✅ Set Global Defaults for Plot Width, Height, and DPI
  options(imf_plot_width = 9.46)
  options(imf_plot_height = 6.85)
  options(imf_dpi = 600)

  # ✅ Inform the user
  packageStartupMessage("imf.bookr package has been loaded.")
}
