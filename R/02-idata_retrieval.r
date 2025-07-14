#-------------------------------------------------------------------------------
# Core Data-Retrieval Functions for IMF iData
#-------------------------------------------------------------------------------

#' Fetch data from IMF Data API via rsdmx, with optional authentication and label resolution.
#'
#' @param department IMF department code (e.g., "BOP")
#' @param dataset    Dataset code (e.g., "BOP_SI")
#' @param key        List or string specifying country/series and frequency
#' @param needs_auth Logical: whether to obtain an Azure OAuth token
#' @param needs_labels Logical: whether to include labels (TRUE) or raw codes (FALSE)
#' @return A cleaned data.frame with standardized TIME_PERIOD and numeric value columns
#' @importFrom AzureAuth get_azure_token
#' @importFrom rsdmx readSDMX
#' @importFrom dplyr select matches
#
imfdata_by_key <- local({
  cache_env  <- new.env(parent = emptyenv())
  client_id  <- "446ce2fa-88b1-436c-b8e6-94491ca4f6fb"
  tenant     <- "https://imfprdb2c.onmicrosoft.com/"
  authority  <- paste0(
    "https://imfprdb2c.b2clogin.com/imfprdb2c.onmicrosoft.com",
    "/b2c_1a_signin_aad_simple_user_journey/oauth2/v2.0"
  )
  scope      <- "https://imfprdb2c.onmicrosoft.com/4042e178-3e2f-4ff9-ac38-1276c901c13d/iData.Login"

  function(department, dataset, key, needs_auth = FALSE, needs_labels = FALSE) {
    # 1) Prepare headers
    if (needs_auth) {
      if (!exists("token", envir = cache_env) ||
          is.null(cache_env$token$credentials$expires_on) ||
          Sys.time() >= as.POSIXct(cache_env$token$credentials$expires_on, origin = "1970-01-01", tz = "UTC")) {
        cache_env$token <- AzureAuth::get_azure_token(
          resource = scope,
          tenant   = tenant,
          app      = client_id,
          version  = 2,
          aad_host = authority
        )
      }
      tok <- cache_env$token$credentials
      headers <- c(
        Authorization = paste(tok$token_type, tok$access_token),
        "User-Agent"  = "idata-script-client"
      )
    } else {
      headers <- c("User-Agent" = "idata-script-client")
    }

    # 2) Construct flowRef
    flowRef <- paste0("IMF.", department, ",", dataset)

    # 3) Retrieve data
    data <- tryCatch({
      args <- list(
        providerId = "IMF_DATA",
        resource   = "data",
        flowRef    = flowRef,
        key        = key,
        headers    = headers
      )
      if (needs_labels) args$dsd <- TRUE
      df <- do.call(rsdmx::readSDMX, args)
      as.data.frame(df, labels = needs_labels)
    }, error = function(e) stop("Failed to retrieve data: ", e$message))

    # 4) Infer frequency
    frequency <- if (is.character(key)) {
      tail(strsplit(key, "\\.")[[1]], 1)
    } else if (is.list(key)) {
      tail(key, 1)[[1]]
    } else stop("Invalid key format: must be character or list")
    if (!frequency %in% c("M", "Q", "A")) stop("Invalid frequency: use 'M', 'Q', or 'A'")

    # 5) Format time
    if ("TIME_PERIOD" %in% names(data)) {
      data <- processTimePeriod(data, frequency)
    } else {
      warning("TIME_PERIOD missing: skipping period processing")
    }

    # 6) Coerce values and clean labels
    if ("OBS_VALUE" %in% names(data)) {
      data$value <- as.numeric(data$OBS_VALUE)
    } else stop("OBS_VALUE missing: cannot extract observations")
    data <- dplyr::select(data, -dplyr::matches("label\\.(ja|fr|zh|ar|ru|pt|es)$"))

    data
  }
})

#' Fetch data for multiple countries and series via imfdata_by_key
#'
#' @param department IMF department code (e.g., "BOP")
#' @param dataset    Dataset code (e.g., "BOP_SI")
#' @param countries  Character vector of country codes (NULL for all)
#' @param series     Character or numeric series code(s)
#' @param frequency  Character: "M", "Q", or "A"
#' @param needs_auth Logical: include authentication header?
#' @param needs_labels Logical: include labels in output?
#' @return A cleaned data.frame
#'
imfdata_by_countries_and_series <- function(
  department,
  dataset,
  countries    = NULL,
  series       = NULL,
  frequency    = "A",
  needs_auth   = FALSE,
  needs_labels = FALSE
) {
  key <- list(
    countries = countries,
    series    = series,
    frequency = frequency
  )
  imfdata_by_key(department, dataset, key, needs_auth, needs_labels)
}

#' List available IMF datasets (dataflows)
#'
#' @param needs_auth Logical: whether to include auth headers
#' @return A data.frame of dataflow metadata
#' @importFrom rsdmx readSDMX
#' @export
imfdata_show_datasets <- function(needs_auth = FALSE) {
  url     <- "https://api.imf.org/external/sdmx/2.1/dataflow?references=none&detail=allstubs"
  headers <- .get_imf_headers(needs_auth)
  df <- rsdmx::readSDMX(url, headers = headers)
  as.data.frame(df)
}

#' Convert a key list to a key string
#'
#' @param key A named list of key components (countries, series, frequency)
#' @return A single string like "US+DE.6.M"
#
make_key_str <- function(key) {
  parts <- vapply(
    key,
    function(el) paste(el, collapse = "+"),
    FUN.VALUE = ""
  )
  paste(parts, collapse = ".")
}
