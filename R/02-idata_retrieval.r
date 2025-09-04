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
    #if ("TIME_PERIOD" %in% names(df)) {
    #  df <- processTimePeriod(df)
    #} else {
    #  warning("TIME_PERIOD missing: skipping period processing")
    #}

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

#' Convert period strings to end-of-period Dates
#'
#' Converts character period labels at daily, monthly, quarterly, or annual
#' frequencies into the corresponding end-of-period `Date`.
#'
#' @param period Character vector of period labels.
#' @param frequency Character vector of frequencies; one of \code{"D"}, \code{"M"},
#'   \code{"Q"}, or \code{"A"} (case-insensitive). If length 1, it is recycled to
#'   match \code{period}.
#'
#' @details
#' Accepted \code{period} formats by \code{frequency}:
#' \itemize{
#'   \item \strong{D} (daily): \code{"YYYY-MM-DD"} (e.g., \code{"2024-02-29"}). Returned date is the same day.
#'   \item \strong{M} (monthly): \code{"YYYY-Mnn"} with \code{nn} in \code{"01"}–\code{"12"}
#'         (e.g., \code{"2024-M01"}). Returns the last calendar day of that month.
#'   \item \strong{Q} (quarterly): \code{"YYYY-Qn"} with \code{n} in \code{1}–\code{4}
#'         (e.g., \code{"2024-Q3"}). Returns the last calendar day of the quarter.
#'   \item \strong{A} (annual): \code{"YYYY"} (e.g., \code{"2024"}). Returns \code{"YYYY-12-31"}.
#' }
#'
#' Inputs are trimmed and uppercased internally. Unsupported frequencies or
#' malformed/empty period strings yield \code{NA}. The function is vectorized
#' over \code{period} and \code{frequency}; if \code{frequency} has length 1 it
#' is recycled. For \code{D}, dates must be ISO \code{"YYYY-MM-DD"}; no other daily
#' formats are parsed.
#'
#' @return A `Date` vector of the same length as \code{period}, with \code{NA} for
#'   invalid inputs.
#'
#' @examples
#' string_to_date_by_freq(c("2024-01-31","2024-02-29"), "D")
#' string_to_date_by_freq(c("2024-M01","2024-M02"), "M")
#' string_to_date_by_freq(c("2024-Q1","2024-Q2"), "Q")
#' string_to_date_by_freq(c("2023","2024"), "A")
#'
#' # Mixed frequencies (vectorized):
#' string_to_date_by_freq(c("2024-Q4","2025"), c("Q","A"))
#'
#' # Invalid formats return NA:
#' string_to_date_by_freq(c("2024-13-01",""), "D")
#'
#' @importFrom lubridate ymd ceiling_date days
#' @export
string_to_date_by_freq <- function(period, frequency) {
  if (!is.character(period)) stop("`period` must be a character vector.")
  if (length(frequency) == 1L) frequency <- rep(frequency, length(period))
  if (length(period) != length(frequency)) {
    stop("`period` and `frequency` must have the same length (or frequency length 1).")
  }

  frequency <- toupper(as.character(frequency))
  out <- vapply(seq_along(period), function(i) {
    s <- toupper(trimws(period[i]))
    f <- frequency[i]

    # Only convert D/M/Q/A; anything else => NA
    if (!f %in% c("D","M","Q","A") || is.na(s) || s == "") return(as.Date(NA))

    if (f == "D") {
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) {
        return(as.Date(lubridate::ymd(s)))
      } else return(as.Date(NA))
    }

    if (f == "A") {
      if (grepl("^\\d{4}$", s)) {
        y <- as.integer(s)
        return(as.Date(sprintf("%04d-12-31", y)))
      } else return(as.Date(NA))
    }

    if (f == "Q") {
      m <- regexec("^(\\d{4})-Q([1-4])$", s); g <- regmatches(s, m)[[1]]
      if (length(g)) {
        y <- as.integer(g[2]); q <- as.integer(g[3]); last_month <- q * 3
        d <- lubridate::ceiling_date(
          lubridate::ymd(sprintf("%04d-%02d-01", y, last_month)), "month"
        ) - lubridate::days(1)
        return(as.Date(d))
      } else return(as.Date(NA))
    }

    if (f == "M") {
      m <- regexec("^(\\d{4})-M(0[1-9]|1[0-2])$", s); g <- regmatches(s, m)[[1]]
      if (length(g)) {
        y <- as.integer(g[2]); mm <- as.integer(g[3])
        d <- lubridate::ceiling_date(
          lubridate::ymd(sprintf("%04d-%02d-01", y, mm)), "month"
        ) - lubridate::days(1)
        return(as.Date(d))
      } else return(as.Date(NA))
    }

    as.Date(NA)
  }, FUN.VALUE = as.Date(NA))

  out
}

