# R/zzz.R
#-------------------------------------------------------------------------------
# Package documentation
#-------------------------------------------------------------------------------

#' imfidatar: Core Data-Retrieval Functions for IMF iData
#'
#' Tools to fetch and clean IMF SDMX data (optionally with auth), build keys,
#' and convert period strings to end-of-period dates.
#'
#' @keywords internal
"_PACKAGE"

#-------------------------------------------------------------------------------
# Hooks
#-------------------------------------------------------------------------------

# This function runs automatically when the package is attached (library(imfidatar))
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("imfidatar loaded.")
}



