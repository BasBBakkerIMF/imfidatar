library(rsdmx)
library(XML)   # for XML fallback

# Reuse your existing DSD fetcher
.fetch_dsd <- function(flow_id, providerId = "IMF_DATA") {
  try1 <- try(
    readSDMX(providerId = providerId, resource = "datastructure",
             resourceId = paste0("DSD_", flow_id), references = "all"),
    silent = TRUE
  )
  if (!inherits(try1, "try-error")) return(try1)

  df_msg <- readSDMX(providerId = providerId, resource = "dataflow")
  flows  <- slot(slot(df_msg, "dataflows"), "dataflows")
  hit    <- Filter(function(f) identical(slot(f, "id"), flow_id), flows)
  if (!length(hit)) stop("Dataflow not found: ", flow_id)
  dsd_id <- slot(hit[[1]], "dsdRef")

  readSDMX(providerId = providerId, resource = "datastructure",
           resourceId = dsd_id, references = "all")
}

# Prefer an English-ish label from a data.frame, if present
.pick_label <- function(df) {
  for (nm in c("label.en","name.en","LABEL.en","NAME.en","label","name")) {
    if (nm %in% names(df)) return(as.character(df[[nm]]))
  }
  rep(NA_character_, nrow(df))
}

# ---- FIXED: accepts a DIMENSION id like "INDEX_TYPE" ----
get_dimension_values <- function(flow_id, dimension, providerId = "IMF_DATA") {
  # 1) map dimension -> codelist id using your working helper
  map  <- get_dimension_codelists(flow_id, providerId)
  cl_id <- map$codelists[match(dimension, map$dimension)]
  if (is.na(cl_id) || !nzchar(cl_id)) {
    stop("No codelist associated with dimension '", dimension,
         "' in flow ", flow_id, " (time dims often have none).")
  }

  # 2) open the same DSD and locate that codelist inside it
  dsd      <- .fetch_dsd(flow_id, providerId)
  cls_obj  <- slot(dsd, "codelists")
  cls_list <- if (!is.null(cls_obj)) slot(cls_obj, "codelists") else list()
  if (!length(cls_list)) stop("This DSD contains no embedded codelists.")

  cl_hits <- Filter(function(cl) identical(slot(cl, "id"), cl_id), cls_list)
  if (!length(cl_hits)) stop("Codelist ", cl_id, " not found in the DSD payload.")
  cl <- cl_hits[[1]]

  # 3) extract codes robustly across rsdmx builds

  # Path A: @Code (common)
  if ("Code" %in% slotNames(cl)) {
    codes <- slot(cl, "Code")
    to_row <- function(code) {
      id  <- slot(code, "id")
      lbl <- if ("label" %in% slotNames(code)) slot(code, "label") else slot(code, "name")
      if (is.list(lbl)) {
        # pick 'en' if available, else first
        lbl <- if ("en" %in% names(lbl)) as.character(lbl[["en"]])
        else as.character(unname(unlist(lbl))[1])
      } else {
        lbl <- as.character(lbl)
      }
      data.frame(code = id, name = lbl, stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, lapply(codes, to_row))
    rownames(out) <- NULL
    return(out)
  }

  # Path B: @codes (some builds)
  if ("codes" %in% slotNames(cl)) {
    codes <- slot(cl, "codes")
    to_row <- function(code) {
      id  <- slot(code, "id")
      lbl <- if ("label" %in% slotNames(code)) slot(code, "label") else slot(code, "name")
      if (is.list(lbl)) {
        lbl <- if ("en" %in% names(lbl)) as.character(lbl[["en"]])
        else as.character(unname(unlist(lbl))[1])
      } else {
        lbl <- as.character(lbl)
      }
      data.frame(code = id, name = lbl, stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, lapply(codes, to_row))
    rownames(out) <- NULL
    return(out)
  }

  # Path C: XML fallback (works when no convenient slots/methods exist)
  doc <- slot(dsd, "xmlObj")   # XMLInternalDocument
  ns  <- c(str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure",
           com="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common",
           xml="http://www.w3.org/XML/1998/namespace")
  nodes <- getNodeSet(doc, sprintf("//str:Codelist[@id='%s']/str:Code", cl_id), namespaces = ns)
  if (!length(nodes)) stop("Codelist ", cl_id, " has no <Code> entries in the XML.")
  code_ids <- vapply(nodes, function(n) xmlGetAttr(n, "id", NA_character_), character(1))
  labels   <- vapply(nodes, function(n) {
    en <- getNodeSet(n, "com:Name[@xml:lang='en']", ns)
    if (length(en)) xmlValue(en[[1]])
    else {
      any <- getNodeSet(n, "com:Name", ns)
      if (length(any)) xmlValue(any[[1]]) else NA_character_
    }
  }, character(1))

  data.frame(code = code_ids, name = labels, stringsAsFactors = FALSE)
}


get_dimension_codelists("CPI", providerId = "IMF_DATA")


get_dimension_values("CPI", "TYPE_OF_TRANSFORMATION", providerId = "IMF_DATA")
