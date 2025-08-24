#'
#'
#'
NULL

# This function runs automatically when the package is attached (library(imf.bookr))
.onAttach <- function(libname, pkgname) {
  # Ensure grid is available for unit()



  # ✅ Inform the user
  packageStartupMessage("imfidatar package has been loaded.")
}
