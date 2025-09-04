#-------------------------------------------------------------------------------
# Core Data-Retrieval Functions for IMF iData
#-------------------------------------------------------------------------------

#' Fetch data from IMF Data API via rsdmx, with optional authentication and label resolution.
#'
#' @param dataset    Dataset code (e.g., "IMF.STA:CPI")
#' @param key        List or string specifying country/series and frequency
#' @param needs_auth Logical: whether to obtain an Azure OAuth token
#' @param needs_labels Logical: whether to include labels (TRUE) or raw codes (FALSE)
#' @return A cleaned data.frame with standardized TIME_PERIOD and numeric value columns
#' @importFrom rsdmx readSDMX
#' @importFrom dplyr select matches
#
imfdata_by_key <- local({
  function(dataset, key,
           needs_auth = FALSE, needs_labels = FALSE) {

    # 1) Prepare headers (via your helper)
    headers <- as.list(.get_imf_headers(needs_auth))

    # 2) Build flowRef
    parts <- strsplit(dataset, ":", fixed = TRUE)[[1]]
    dept  <- parts[1]
    id    <- parts[2]
    flowRef <- paste0(dept, ",", id)

    # 3) Fetch via rsdmx
    df <- tryCatch({
      args <- list(
        providerId = "IMF_DATA",
        resource   = "data",
        flowRef    = flowRef,
        key        = key,
        headers    = headers
      )
      if (needs_labels) args$dsd <- TRUE

      raw <- do.call(rsdmx::readSDMX, args)
      as.data.frame(raw, labels = needs_labels)
    }, error = function(e) {
      stop("Failed to retrieve data: ", e$message)
    })

    # 4) Infer frequency
    # freq <- if (is.character(key)) {
    #   tail(strsplit(key, "\\.")[[1]], 1)
    # } else if (is.list(key)) {
    #   tail(key, 1)[[1]]
    # } else stop("Invalid key format: must be character or list")
    #
    # if (!freq %in% c("M", "Q", "A","D")) stop("Invalid frequency: use 'M', 'Q', or 'A'")
    #
    # 5) Process TIME_PERIOD
    if ("TIME_PERIOD" %in% names(df)) {
      df <- processTimePeriod(df)
    } else {
      warning("TIME_PERIOD missing: skipping period processing")
    }

    # 6) Coerce and clean up
    if (!"OBS_VALUE" %in% names(df)) {
      stop("OBS_VALUE missing: cannot extract observations")
    }
    df$value <- as.numeric(df$OBS_VALUE)
    df <- dplyr::select(df, -dplyr::matches("label\\.(ja|fr|zh|ar|ru|pt|es)$"))

    df
  }
})

#' Fetch data for multiple countries and series via imfdata_by_key
#'
#' @param dataset    Dataset code (e.g., "BOP_SI")
#' @param countries  Character vector of country codes (NULL for all)
#' @param series     Character or numeric series code(s)
#' @param frequency  Character: "M", "Q", or "A"
#' @param needs_auth Logical: include authentication header?
#' @param needs_labels Logical: include labels in output?
#' @return A cleaned data.frame
#'
imfdata_by_countries_and_series <- function(
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
  imfdata_by_key(dataset, key, needs_auth, needs_labels)
}

#' Build an environment mapping Name.en -> "agencyID:id" (alphabetical)
#'
#' @param df A data.frame with at least columns `id`, `agencyID`, and `Name.en`
#' @return An environment where each Name.en is a symbol whose value is "agencyID:id"
#'
make_dataset_env <- function(needs_auth = FALSE) {
  df <- imfdata_show_datasets(needs_auth = needs_auth)
  stopifnot(all(c("id","agencyID","Name.en") %in% names(df)))

  vals <- with(df, paste0(agencyID, ":", id))
  names(vals) <- df$Name.en
  vals <- vals[order(names(vals))]

  env <- new.env(parent = emptyenv())
  list2env(as.list(vals), envir = env)
  env
}

#' Show available IMF dataflows (with optional B2C auth)
#'
#' @param needs_auth Logical(1). Whether to add an OAuth token header.
#' @return A data.frame of available dataflows.
imfdata_show_datasets <- local({
  function(needs_auth = FALSE) {
    headers <- as.list(.get_imf_headers(needs_auth))
    url <- "https://api.imf.org/external/sdmx/2.1/dataflow?references=none&detail=allstubs"
    df  <- rsdmx::readSDMX(url, headers = headers)
    as.data.frame(df)
  }
})

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

#-------------------------------------------------------------------------------
# Date‐conversion utilities for IMF iData package
#-------------------------------------------------------------------------------

#' Convert a "YYYY-MM" string to the last date of that month
#'
#' @param month_string Character(1). Year-month string in "YYYY-MM" or "YYYY-MM-XX" format.
#' @return Date. Last day of the specified month.
#' @importFrom lubridate ymd ceiling_date days
#' @noRd
yearMonthToDate <- function(month_string) {
  if (is.null(month_string) || nchar(month_string) < 7) {
    stop("Invalid month string format: must be at least 'YYYY-MM'")
  }
  year  <- as.numeric(substr(month_string, 1, 4))
  month <- as.numeric(substr(month_string, 6, 7))
  ceiling_date(ymd(paste0(year, "-", sprintf("%02d", month), "-01")), "month") - days(1)
}

#' Convert a "YYYY-Q" string to the last date of that quarter
#'
#' @param quarter_string Character(1). Year-quarter string in "YYYY-Q" or "YYYY-QX" format.
#' @return Date. Last day of the specified quarter.
#' @importFrom lubridate ymd ceiling_date days
#' @noRd
yearQuarterToDate <- function(quarter_string) {
  if (is.null(quarter_string) || nchar(quarter_string) < 6) {
    stop("Invalid quarter string format: must be at least 'YYYY-Q'")
  }
  year    <- as.numeric(substr(quarter_string, 1, 4))
  quarter <- as.numeric(substr(quarter_string, 6, 6))
  if (!quarter %in% 1:4) {
    stop("Invalid quarter: must be 1, 2, 3, or 4")
  }
  last_month <- quarter * 3
  ceiling_date(ymd(paste0(year, "-", sprintf("%02d", last_month), "-01")), "month") - days(1)
}

#' Process TIME_PERIOD column into date or numeric values
#'
#' @param dataset    Data frame with a "TIME_PERIOD" column
#' @param frequency  Character(1). One of "M", "Q", or "A"
#' @return Data frame. Adds a "date" column (Date) for "M"/"Q" or numeric for "A".
#' @importFrom dplyr mutate
#' @noRd
processTimePeriod <- function(dataset) {
  if (!"TIME_PERIOD" %in% colnames(dataset)) {
    stop("TIME_PERIOD column is missing in the dataset.")
  }
  # frequency=dataset[1,"FREQUENCY"]
  # if (frequency == "Q") {
  #   dataset$date <- as.Date(sapply(dataset$TIME_PERIOD, yearQuarterToDate))
  # } else if (frequency == "M") {
  #   dataset$date <- as.Date(sapply(dataset$TIME_PERIOD, yearMonthToDate))
  # } else if (frequency == "A") {
  #   dataset$date <- as.numeric(dataset$TIME_PERIOD)
  # }
  dataset
}
