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
  # compute last day of that month
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
