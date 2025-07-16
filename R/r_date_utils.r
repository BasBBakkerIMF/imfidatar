#-------------------------------------------------------------------------------
# Date‐conversion utilities for IMF iData package
#-------------------------------------------------------------------------------

#' Convert a "YYYY-MM" string to the last date of that month
#'
#' @param month_string Character(1). Year-month string in "YYYY-MM" or "YYYY-MM-XX" format.
#' @return Date. Last day of the specified month.
#' @importFrom lubridate ymd ceiling_date days
#' @noRd
library(lubridate)

yearMonthToDate <- function(month_string) {
  # check format
  if (any(!grepl("^\\d{4}-M\\d{2}$", month_string))) {
    stop("Invalid month string format: must be 'YYYY-MMM', e.g. '1955-M01'")
  }
  # replace "-M" with "-" so we get "YYYY-MM"
  ym_clean <- sub("-M", "-", month_string)
  # parse first-of-month dates
  first_of_month <- ymd(paste0(ym_clean, "-01"))
  # ceiling to next month, then back one day
  last_of_month <- ceiling_date(first_of_month, "month") - days(1)
  return(last_of_month)
}


#' Convert a "YYYY-Q" string to the last date of that quarter
#'
#' @param quarter_string Character(1). Year-quarter string in "YYYY-Q" or "YYYY-QX" format.
#' @return Date. Last day of the specified quarter.
#' @importFrom lubridate ymd ceiling_date days
#' @noRd
yearQuarterToDate <- function(quarter_string) {
  # must be a character vector, no NAs
  if (!is.character(quarter_string)) {
    stop("`quarter_string` must be a character vector, e.g. c(\"1995-Q1\",\"2000-Q4\").")
  }
  if (any(is.na(quarter_string))) {
    stop("`quarter_string` contains NA; please remove or impute missing quarters.")
  }

  # strict regex: 4 digits, “-Q”, then 1–4
  valid <- grepl("^\\d{4}-Q[1-4]$", quarter_string)
  if (any(!valid)) {
    stop(
      "Invalid format at positions: ",
      paste(which(!valid), collapse = ", "),
      ".\nEach element must look like “YYYY-Qn”, e.g. “2025-Q2”."
    )
  }

  # extract year and quarter (note: quarter digit is at position 7)
  years    <- as.integer(substr(quarter_string, 1, 4))
  quarters <- as.integer(substr(quarter_string, 7, 7))

  # last month of each quarter
  last_month <- quarters * 3

  # first day of that last month
  first_of_last <- ymd(paste0(years, "-", sprintf("%02d", last_month), "-01"))

  # roll forward to first of next month, then step back one day
  last_of_quarter <- ceiling_date(first_of_last, "month") - days(1)

  return(last_of_quarter)
}


#' Process TIME_PERIOD column into date or numeric values
#'
#' @param dataset    Data frame with a "TIME_PERIOD" column
#' @param frequency  Character(1). One of "M", "Q", or "A"
#' @return Data frame. Adds a "date" column (Date) for "M"/"Q" or numeric for "A".
#' @importFrom dplyr mutate
#' @noRd
processTimePeriod <- function(dataset, frequency) {
  if (!"TIME_PERIOD" %in% colnames(dataset)) {
    stop("TIME_PERIOD column is missing in the dataset.")
  }
  if (length(frequency) != 1 || !frequency %in% c("M", "Q", "A")) {
    stop("frequency must be one of 'M', 'Q', or 'A'")
  }
  if (frequency == "Q") {
    dataset$date <- as.Date(sapply(dataset$TIME_PERIOD, yearQuarterToDate))
  } else if (frequency == "M") {
    dataset$date <- as.Date(sapply(dataset$TIME_PERIOD, yearMonthToDate))
  } else if (frequency == "A") {
    dataset$date <- as.numeric(dataset$TIME_PERIOD)
  }
  dataset
}
