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
#' @importFrom AzureAuth get_azure_token
#' @importFrom rsdmx readSDMX
#' @importFrom dplyr select matches
#
imfdata_by_key <- local({
  # private cache for tokens

  #print("there we are")
  cache_env <- new.env(parent = emptyenv())


  # your Azure B2C app details
  client_id  <- "446ce2fa-88b1-436c-b8e6-94491ca4f6fb"
  tenant     <- "https://imfprdb2c.onmicrosoft.com/"
  authority  <- "https://imfprdb2c.b2clogin.com/imfprdb2c.onmicrosoft.com/b2c_1a_signin_aad_simple_user_journey/oauth2/v2.0"
  scope      <- "https://imfprdb2c.onmicrosoft.com/4042e178-3e2f-4ff9-ac38-1276c901c13d/iData.Login"

  # helper to fetch/refresh token
  get_new_token <- function() {
    AzureAuth::get_azure_token(
      resource = c(scope, "offline_access"),
      tenant   = tenant,
      app      = client_id,
      version  = 2,
      aad_host = authority
    )
  }

  # the actual function
  function(dataset, key,
           needs_auth = FALSE, needs_labels = FALSE) {
    # 1) Prepare headers
    if (needs_auth) {

   print("needs auth")
      token_creds <- cache_env$token$credentials
      exp_time    <- if (is.numeric(token_creds$expires_on)) {
        as.POSIXct(token_creds$expires_on, origin="1970-01-01", tz="UTC")
      } else if (inherits(token_creds$expires_on, "POSIXct")) {
        token_creds$expires_on
      } else {
        as.POSIXct(token_creds$expires_on, tz="UTC")
      }

      if (!exists("token", envir=cache_env) || Sys.time() >= exp_time) {
        cache_env$token <- get_new_token()
      }






      # fetch or refresh token
      if (!exists("token", envir = cache_env) ||
          is.null(cache_env$token) ||
          is.null(cache_env$token$credentials$expires_on) ||
          Sys.time() >= as.POSIXct(cache_env$token$credentials$expires_on,
                                   origin = "1970-01-01", tz = "UTC")) {
        cache_env$token <- get_new_token()
      }
      tok <- cache_env$token$credentials
      headers <- c(
        Authorization = paste(tok$token_type, tok$access_token),
        `User-Agent`  = "idata-script-client"
      )
    } else {
      headers <- c(`User-Agent` = "idata-script-client")
    }

    # 2) Build flowRef

    parts <- strsplit(dataset, ":", fixed = TRUE)[[1]]

    # assign
    dept <- parts[1]
    id   <- parts[2]


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
    freq <- if (is.character(key)) {
      tail(strsplit(key, "\\.")[[1]], 1)
    } else if (is.list(key)) {
      tail(key, 1)[[1]]
    } else stop("Invalid key format: must be character or list")

    if (!freq %in% c("M", "Q", "A","D")) stop("Invalid frequency: use 'M', 'Q', or 'A'")

    # 5) Process TIME_PERIOD
    if ("TIME_PERIOD" %in% names(df)) {
      df <- processTimePeriod(df, freq)
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
make_dataset_env <- function(needs_auth=F) {

  df<-imfdata_show_datasets(needs_auth = needs_auth)
  stopifnot(all(c("id","agencyID","Name.en") %in% names(df)))

  # build a named vector of "agencyID:id"
  vals <- with(df, paste0(agencyID, ":", id))
  names(vals) <- df$Name.en

  # sort by the names (Name.en)
  vals <- vals[order(names(vals))]

  # create and populate the environment
  env <- new.env(parent = emptyenv())
  list2env(as.list(vals), envir = env)
  env
}






#' Show available IMF dataflows (with optional B2C auth)
#'
#' @param needs_auth Logical(1). Whether to add an OAuth token header.
#' @return A data.frame of available dataflows.
#' @importFrom AzureAuth get_azure_token
imfdata_show_datasets <- local({
  # private cache for tokens
  cache_env <- new.env(parent = emptyenv())

  # your Azure B2C app details
  client_id  <- "446ce2fa-88b1-436c-b8e6-94491ca4f6fb"
  tenant     <- "https://imfprdb2c.onmicrosoft.com/"
  authority  <- "https://imfprdb2c.b2clogin.com/imfprdb2c.onmicrosoft.com/b2c_1a_signin_aad_simple_user_journey/oauth2/v2.0"
  scope      <- "https://imfprdb2c.onmicrosoft.com/4042e178-3e2f-4ff9-ac38-1276c901c13d/iData.Login"

  # helper to fetch/refresh token
  get_new_token <- function() {
    AzureAuth::get_azure_token(
      resource = c(scope, "offline_access"),  # ← vector of scopes
      tenant   = tenant,
      app      = client_id,
      version  = 2,
      aad_host = authority
    )
  }


  function(needs_auth = FALSE) {
    if (needs_auth) {
      # get or refresh token
      if (!exists("token", envir = cache_env) ||
          is.null(cache_env$token) ||
          is.null(cache_env$token$credentials$expires_on) ||
          Sys.time() >= as.POSIXct(
            cache_env$token$credentials$expires_on,
            origin = "1970-01-01", tz = "UTC"
          )) {
        cache_env$token <- get_new_token()
      }
      tok <- cache_env$token$credentials
      headers <- c(
        Authorization = paste(tok$token_type, tok$access_token),
        `User-Agent`  = "idata-script-client"
      )
    } else {
      headers <- c(`User-Agent` = "idata-script-client")
    }
    # fetch dataflow metadata
    #print(headers)
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
