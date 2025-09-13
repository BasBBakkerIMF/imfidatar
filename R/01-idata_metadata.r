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
.get_raw_dsd <- function(dataset,needs_auth=F) {
  if (!exists(".dsdcache", envir = .GlobalEnv)) {
    assign(".dsdcache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  if (exists(dataset, envir = .dsdcache, inherits = FALSE)) {
    return(get(dataset, envir = .dsdcache, inherits = FALSE))
  }

  # --- added: build and pass HTTP headers ---
  sdmx_headers <- as.list(c(
    .get_imf_headers(needs_auth = needs_auth),
    Accept = "application/vnd.sdmx.structure+xml;version=2.1"
  ))
  # ------------------------------------------

  dsd <- rsdmx::readSDMX(
    providerId = "IMF_DATA",
    resource   = "datastructure",
    resourceId = dataset,
    headers    = .get_imf_headers()
  )
  assign(dataset, dsd, envir = .dsdcache)
  dsd
}

#' Get dimension names from a DSD or IMF dataflow string
#'
#' @param dataset_or_db Character(1). Either an IMF dataset identifier
#'   (e.g. "DSD_BOP_SI") or a dataflow string (e.g. "IMF.STA:CPI").
#' @return A data.frame with a single column `Dimension` listing dimension names.
#' @importFrom methods slot
#' @importFrom stringr str_split

get_dimension_names <- function(dataset_or_db,needs_auth=F) {
  # If the input contains a ":", treat it as a dataflow and extract the code part
  if (grepl(":", dataset_or_db)) {
    parts  <- stringr::str_split(dataset_or_db, ":", simplify = TRUE)
    raw    <- parts[2]
    dsd_id <- if (grepl("^DSD_", raw)) raw else paste0("DSD_", raw)
  } else {
    dsd_id <- dataset_or_db
  }

  #print(dsd_id)
  # Fetch (or cached) the raw DSD
  dsd <- .get_raw_dsd(dsd_id,needs_auth = needs_auth)

  # Extract the dimensions
  ds   <- slot(dsd, "datastructures")@datastructures[[1]]
  dims <- slot(ds,    "Components")@Dimensions

  # Return as data.frame
  df<-data.frame(
    Dimension = vapply(dims, function(x) slot(x, "conceptRef"), FUN.VALUE = ""),
    stringsAsFactors = FALSE
  )
  print(df)
  return(df)
}
#' Get codes and labels for a specific dimension in a DSD or IMF dataflow string
#'
#' Extracts all code–label pairs for the specified dimension from the dataset's DSD.
#'
#' @param dataset_or_db Character(1). Either an IMF dataset identifier
#'   (e.g. "DSD_BOP_SI") or a dataflow string (e.g. "IMF.STA:CPI").
#' @param dimension_id  Character(1). The dimension identifier to retrieve
#'                      (e.g. "COUNTRY").
#' @param language      Character(1). Language code for labels (default: "en").
#' @return A tibble with row names as codes and a column `Label` with labels.
#' @importFrom tibble tibble column_to_rownames
#' @importFrom methods slot
#' @importFrom stringr str_split
get_dimension_values <- function(dataset_or_db, dimension_id, language = "en") {
  # normalize to a DSD ID
  if (grepl(":", dataset_or_db)) {
    parts  <- stringr::str_split(dataset_or_db, ":", simplify = TRUE)
    raw    <- parts[2]
    dsd_id <- if (grepl("^DSD_", raw)) raw else paste0("DSD_", raw)
  } else {
    dsd_id <- dataset_or_db
  }

  # fetch the DSD object (cached if available)
  dsd <- .get_raw_dsd(dsd_id)

  # pull all codelists and find the one matching our dimension
  all_codelists <- slot(dsd, "codelists")@codelists
  cl_list <- Filter(
    function(cl) endsWith(slot(cl, "id"), dimension_id),
    all_codelists
  )
  if (length(cl_list) == 0) {
    stop("No codelist found for dimension: ", dimension_id)
  }
  cl <- cl_list[[1]]

  # extract codes and labels
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

  # set codes as row names
  tibble::column_to_rownames(df, var = "Code")
}

#' Create an environment mapping dimension labels to codes
#'
#' @param dataset_or_db  Character(1). Either an IMF dataflow string
#'                       (e.g. "IMF.STA:CPI") or a DSD ID
#'                       (e.g. "DSD_BOP_SI").
#' @param dimension      Character(1). Dimension name,
#'                       e.g. "TYPE_OF_TRANSFORMATION".
#' @return Environment: names = labels, values = codes
#' @importFrom stringr str_split
make_dimension_env <- function(dataset_or_db, dimension) {
  # normalize to a DSD ID
  if (grepl(":", dataset_or_db)) {
    parts  <- stringr::str_split(dataset_or_db, ":", simplify = TRUE)
    raw    <- parts[2]
    dsd_id <- if (grepl("^DSD_", raw)) raw else paste0("DSD_", raw)
  } else {
    dsd_id <- dataset_or_db
  }

  # reuse the updated get_dimension_values(), which accepts both forms:
  df <- get_dimension_values(dsd_id, dimension)

  # rename the label column and capture codes
  colnames(df) <- dimension
  df$code      <- rownames(df)

  # build the named list and then an env
  vals <- setNames(as.list(df$code), df[[dimension]])
  env  <- new.env(parent = emptyenv())
  list2env(vals, envir = env)
  env
}
