get_codelist_names <- function(dataset_or_db, needs_auth = FALSE) {
  dsd_id <- .normalize_dsd_id(dataset_or_db)
  dsd    <- .get_raw_dsd(dsd_id, needs_auth = needs_auth)
  cls    <- methods::slot(dsd, "codelists")@codelists

  tibble::tibble(
    CodelistID = vapply(cls, function(cl) methods::slot(cl, "id"), FUN.VALUE = "")
  )
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

#-----

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
