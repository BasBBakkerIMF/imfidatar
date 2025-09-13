#-------------------------------------------------------------------------------
# SDMX DSD helpers (IMF / rsdmx)
#-------------------------------------------------------------------------------

if (getRversion() >= "2.15.1") utils::globalVariables(c(".dsdcache", ".codelistcache"))

#' @keywords internal
#' @importFrom methods slot
NULL

#-------------------------------------------------------------------------------
# Internals
#-------------------------------------------------------------------------------

# Ensure a named cache env exists in .GlobalEnv
.ensure_cache_env <- function(name) {
  if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    assign(name, new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(name, envir = .GlobalEnv, inherits = FALSE)
}

# Normalize user input ("IMF.STA:CPI" or "DSD_CPI") to a DSD id like "DSD_CPI"
.normalize_dsd_id <- function(dataset_or_db) {
  if (grepl(":", dataset_or_db)) {
    parts  <- stringr::str_split(dataset_or_db, ":", simplify = TRUE)
    raw    <- parts[2]
    if (grepl("^DSD_", raw)) raw else paste0("DSD_", raw)
  } else {
    dataset_or_db
  }
}

# Normalize a label to the requested language
.label_for_lang <- function(lab, language = "en") {
  if (is.list(lab)) {
    if (language %in% names(lab)) return(as.character(lab[[language]]))
    return(as.character(unname(unlist(lab))[1]))
  }
  as.character(lab)
}

# Cache key for codelists: DSD + language
.codelist_key <- function(dsd_id, language) paste0(dsd_id, "::", language)

#-------------------------------------------------------------------------------
# Load/cache DSD
#-------------------------------------------------------------------------------

#' Load or cache the raw DSD object for a given dataset
#'
#' Fetches the SDMX DataStructureDefinition for the specified dataset from the
#' IMF provider and caches it in `.dsdcache` for subsequent calls.
#'
#' @param dataset Character(1). IMF DSD identifier (e.g., "DSD_BOP_SI").
#' @param needs_auth Logical. Whether authenticated headers are needed.
#' @return An object of class `SDMX` representing the DSD.
#' @importFrom rsdmx readSDMX
#' @noRd
.get_raw_dsd <- function(dataset, needs_auth = FALSE) {
  dsd_cache <- .ensure_cache_env(".dsdcache")

  # return cached
  if (exists(dataset, envir = dsd_cache, inherits = FALSE)) {
    return(get(dataset, envir = dsd_cache, inherits = FALSE))
  }

  # Build headers (use exported get_imf_headers())
  sdmx_headers <- as.list(c(
    get_imf_headers(needs_auth = needs_auth),
    Accept = "application/vnd.sdmx.structure+xml;version=2.1"
  ))

  dsd <- rsdmx::readSDMX(
    providerId = "IMF_DATA",
    resource   = "datastructure",
    resourceId = dataset,
    headers    = sdmx_headers
  )

  assign(dataset, dsd, envir = dsd_cache)
  dsd
}

#-------------------------------------------------------------------------------
# Dimensions
#-------------------------------------------------------------------------------

#' Get dimension names from a DSD or IMF dataflow string
#'
#' @param dataset_or_db Character(1). An IMF dataset id (e.g. "DSD_BOP_SI")
#'   or a dataflow string (e.g. "IMF.STA:CPI").
#' @param needs_auth Logical. Pass-through for IMF headers.
#' @return A tibble with a single column `Dimension`.
#' @importFrom methods slot
#' @importFrom stringr str_split
get_dimension_names <- function(dataset_or_db, needs_auth = FALSE) {
  dsd_id <- .normalize_dsd_id(dataset_or_db)
  dsd    <- .get_raw_dsd(dsd_id, needs_auth = needs_auth)

  ds   <- methods::slot(dsd, "datastructures")@datastructures[[1]]
  dims <- methods::slot(ds,  "Components")@Dimensions

  tibble::tibble(
    Dimension = vapply(dims, function(x) methods::slot(x, "conceptRef"), FUN.VALUE = "")
  )
}

#' Get codes and labels for a specific dimension in a DSD/dataflow (tidy tibble)
#'
#' @param dataset_or_db Character(1). DSD id (e.g. "DSD_BOP_SI") or dataflow (e.g. "IMF.STA:CPI").
#' @param dimension_id  Character(1). Dimension id (e.g. "COUNTRY").
#' @param language      Character(1). Language code for labels (default: "en").
#' @param needs_auth    Logical. Pass-through for IMF headers.
#' @return A tibble with columns `Code`, `Label`.
#' @importFrom tibble tibble
#' @importFrom methods slot
#' @importFrom stringr str_split
get_dimension_values <- function(dataset_or_db, dimension_id, language = "en", needs_auth = FALSE) {
  dsd_id <- .normalize_dsd_id(dataset_or_db)
  dsd    <- .get_raw_dsd(dsd_id, needs_auth = needs_auth)

  cls <- methods::slot(dsd, "codelists")@codelists
  # find codelist whose id ends with the dimension id
  cl_list <- Filter(function(cl) endsWith(methods::slot(cl, "id"), dimension_id), cls)
  if (length(cl_list) == 0) stop("No codelist found for dimension: ", dimension_id)

  codes_all <- methods::slot(cl_list[[1]], "Code")
  tibble::tibble(
    Code  = vapply(codes_all, function(cd) methods::slot(cd, "id"), FUN.VALUE = ""),
    Label = vapply(codes_all, function(cd) .label_for_lang(methods::slot(cd, "label"), language), FUN.VALUE = "")
  )
}

#-------------------------------------------------------------------------------
# Codelists (memoized tibbles)


#-------------------------------------------------------------------------------
# Find / list codelists
#-------------------------------------------------------------------------------

#' Find a codelist in a DSD by its id (exact or suffix match)
#'
#' @param dataset_or_db Character(1). IMF dataflow (e.g. "IMF.STA:CPI")
#'   or DSD id (e.g. "DSD_CPI").
#' @param codelist_id   Character(1). e.g. "CL_FREQ", "CL_AREA", or DSD-prefixed id.
#' @param language      Character(1). Label language (default "en").
#' @param needs_auth    Logical. Passed through for IMF headers.
#' @param return        One of "tibble", "object", or "id".
#' @return Depending on `return`, a tibble (Code, Label), the SDMX codelist object, or the id string.
get_codelist_values_by_id <- function(dataset_or_db,
                          codelist_id,
                          language   = "en",
                          needs_auth = FALSE,
                          return     = c("tibble", "object", "id")) {
  return <- match.arg(return)

  dsd_id <- .normalize_dsd_id(dataset_or_db)
  dsd    <- .get_raw_dsd(dsd_id, needs_auth = needs_auth)

  cls <- methods::slot(dsd, "codelists")@codelists
  if (!length(cls)) stop("No codelists present in DSD: ", dsd_id)

  cl_ids <- vapply(cls, function(cl) methods::slot(cl, "id"), FUN.VALUE = "")

  hit_idx <- which(cl_ids == codelist_id)
  if (!length(hit_idx)) hit_idx <- which(endsWith(cl_ids, codelist_id))

  if (!length(hit_idx)) {
    stop("Codelist not found: '", codelist_id,
         "'. Available ids include: ",
         paste(head(cl_ids, 20), collapse = ", "),
         if (length(cl_ids) > 20) ", …" else "")
  }
  if (length(hit_idx) > 1) {
    stop("Ambiguous codelist id '", codelist_id, "'. Matches: ",
         paste(cl_ids[hit_idx], collapse = ", "),
         ". Please specify the full codelist id.")
  }

  resolved_id <- cl_ids[hit_idx]
  cl_obj <- cls[[hit_idx]]

  if (return == "id")     return(resolved_id)
  if (return == "object") return(cl_obj)

  codes <- methods::slot(cl_obj, "Code")
  tibble::tibble(
    Code  = vapply(codes, function(cd) methods::slot(cd, "id"), FUN.VALUE = ""),
    Label = vapply(codes, function(cd) .label_for_lang(methods::slot(cd, "label"), language), FUN.VALUE = "")
  )
}

#' Return a tibble with only the codelist IDs
#' @return tibble with one column: CodelistID
get_codelist_names <- function(dataset_or_db, needs_auth = FALSE) {
  dsd_id <- .normalize_dsd_id(dataset_or_db)
  dsd    <- .get_raw_dsd(dsd_id, needs_auth = needs_auth)
  cls    <- methods::slot(dsd, "codelists")@codelists

  tibble::tibble(
    CodelistID = vapply(cls, function(cl) methods::slot(cl, "id"), FUN.VALUE = "")
  )
}

#-------------------------------------------------------------------------------
# Dimension label→code environment
#-------------------------------------------------------------------------------

#' Create an environment mapping dimension labels to codes
#'
#' @param dataset_or_db Character(1). IMF dataflow string (e.g. "IMF.STA:CPI")
#'                      or DSD ID (e.g. "DSD_BOP_SI").
#' @param dimension     Character(1). Dimension name, e.g. "TYPE_OF_TRANSFORMATION".
#' @param language      Character(1). Label language for the mapping (default "en").
#' @param needs_auth    Logical. Pass-through for IMF headers.
#' @return Environment: names = labels, values = codes
make_dimension_env <- function(dataset_or_db, dimension, language = "en", needs_auth = FALSE) {
  df <- get_dimension_values(dataset_or_db, dimension, language = language, needs_auth = needs_auth)

  # De-duplicate labels to keep a valid env mapping
  vals <- setNames(as.list(df$Code), make.unique(df$Label, sep = "_"))
  env  <- new.env(parent = emptyenv())
  list2env(vals, envir = env)
  env
}
