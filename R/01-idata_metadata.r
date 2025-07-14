#-------------------------------------------------------------------------------
# Helper functions for SDMX Data Structure Definitions (DSDs)
#-------------------------------------------------------------------------------

if (getRversion() >= "2.15.1") utils::globalVariables(".dsdcache")

#' @keywords internal
#' @importFrom methods slot
NULL

#-------------------------------------------------------------------------------
# Helper functions for SDMX Data Structure Definitions (DSDs)
#-------------------------------------------------------------------------------

#' Load or cache the raw DSD object for a given dataset
#'
#' This internal function fetches the SDMX DataStructureDefinition for the specified
#' dataset from the IMF provider and caches it in `.dsdcache` for subsequent calls.
#'
#' @param dataset Character(1). IMF dataset identifier (e.g., "DSD_BOP_SI").
#' @return An object of class `SDMX` representing the DSD.
#' @importFrom rsdmx readSDMX
#' @noRd
.get_raw_dsd <- function(dataset) {
  if (!exists(".dsdcache", envir = .GlobalEnv)) {
    assign(".dsdcache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  if (exists(dataset, envir = .dsdcache, inherits = FALSE)) {
    return(get(dataset, envir = .dsdcache, inherits = FALSE))
  }
  dsd <- rsdmx::readSDMX(
    providerId = "IMF_DATA",
    resource   = "datastructure",
    resourceId = dataset
  )
  assign(dataset, dsd, envir = .dsdcache)
  dsd
}

#' Get dimension names from a DSD
#'
#' Retrieves the list of dimension identifiers defined in the DSD for the given dataset.
#'
#' @param dataset Character(1). IMF dataset identifier (e.g., "DSD_BOP_SI").
#' @return A data.frame with a single column `Dimension` listing dimension names.
#' @importFrom methods slot
#
get_dimension_names <- function(dataset) {
  dsd <- .get_raw_dsd(dataset)
  ds  <- slot(dsd, "datastructures")@datastructures[[1]]
  dims <- slot(ds, "Components")@Dimensions
  data.frame(
    Dimension = vapply(dims, function(x) slot(x, "conceptRef"), FUN.VALUE = ""),
    stringsAsFactors = FALSE
  )
}

#' Get codes and labels for a specific dimension in a DSD
#'
#' Extracts all code-label pairs for the specified dimension from the dataset's DSD.
#'
#' @param dataset Character(1). IMF dataset identifier (e.g., "DSD_BOP_SI").
#' @param dimension_id Character(1). The dimension identifier to retrieve (e.g., "COUNTRY").
#' @param language Character(1). Language code for labels (default: "en").
#' @return A tibble with row names as codes and a column `Label` with labels.
#' @importFrom tibble tibble column_to_rownames
#' @importFrom methods slot
#' @export
get_dimension_values <- function(dataset, dimension_id, language = "en") {
  dsd <- .get_raw_dsd(dataset)
  all_codelists <- slot(dsd, "codelists")@codelists
  cl_list <- Filter(
    function(cl) endsWith(slot(cl, "id"), dimension_id),
    all_codelists
  )
  if (length(cl_list) == 0) {
    stop("No codelist found for dimension: ", dimension_id)
  }
  cl <- cl_list[[1]]
  codes_all <- slot(cl, "Code")
  df <- tibble::tibble(
    Code  = vapply(codes_all, function(cd) slot(cd, "id"), FUN.VALUE = ""),
    Label = vapply(codes_all, function(cd) {
      lab <- slot(cd, "label")
      if (is.list(lab) && language %in% names(lab)) {
        as.character(lab[[language]])
      } else if (is.list(lab)) {
        unname(unlist(lab))[1]
      } else {
        as.character(lab)
      }
    }, FUN.VALUE = "")
  )
  df <- tibble::column_to_rownames(df, var = "Code")
  df
}

#' Create an environment mapping dimension labels to codes
#'
#' @param db        Full SDMX dataflow string, e.g. "IMF.STA:CPI"
#' @param dimension Dimension name, e.g. "TYPE_OF_TRANSFORMATION"
#' @return Environment: names = labels, values = codes
#' @importFrom stringr str_split
#' @export
get_dimension_env <- function(db, dimension) {
  parts  <- stringr::str_split(db, ":", simplify = TRUE)
  raw    <- parts[2]
  # if the string doesn’t already start with "DSD_", add it:
  dsd_id <- if (grepl("^DSD_", raw)) raw else paste0("DSD_", raw)

  df <- get_dimension_values(dsd_id, dimension)
  colnames(df) <- dimension
  df$code      <- rownames(df)

  vals <- as.list(df$code)
  names(vals) <- df[[dimension]]

  env <- new.env(parent = emptyenv())
  list2env(vals, envir = env)
  env
}

