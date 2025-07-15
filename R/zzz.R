#'
#'
#'
NULL

# This function runs automatically when the package is attached (library(imf.bookr))
.onAttach <- function(libname, pkgname) {
  # Ensure grid is available for unit()
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required but not installed.")
  }



  # ✅ Inform the user
  packageStartupMessage("imfidata package has been loaded.")
}
