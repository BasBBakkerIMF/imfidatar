#’ -----------------------------------------------------------------------------
#’ Internal helper: build HTTP headers for IMF API calls
#’
#’ @param needs_auth Logical(1). If FALSE, returns only a User-Agent header;
#’                   if TRUE, acquires or refreshes an Azure AD token and
#’                   returns both Authorization and User-Agent headers.
#’ @return Named character vector of HTTP headers.
#’ @seealso [AzureAuth::get_azure_token()]
#’ @importFrom AzureAuth get_azure_token
#’ @noRd
#’ -----------------------------------------------------------------------------
.get_imf_headers <- local({
  env <- new.env(parent = emptyenv())
  client_id <- "446ce2fa-88b1-436c-b8e6-94491ca4f6fb"
  tenant    <- "https://imfprdb2c.onmicrosoft.com/"
  authority <- "https://imfprdb2c.b2clogin.com/imfprdb2c.onmicrosoft.com/
                b2c_1a_signin_aad_simple_user_journey/oauth2/v2.0"
  scope     <- "https://imfprdb2c.onmicrosoft.com/
                4042e178-3e2f-4ff9-ac38-1276c901c13d/iData.Login"

  function(needs_auth) {
    if (identical(needs_auth, FALSE)) {
      return(c("User-Agent" = "idata-script-client"))
    }
    # Acquire or refresh token if missing or expired
    if (!exists("token", envir = env) ||
        Sys.time() >= as.POSIXct(env$token$credentials$expires_on,
                                 origin = "1970-01-01", tz = "UTC")) {
      env$token <- AzureAuth::get_azure_token(
        resource  = scope,
        tenant    = tenant,
        app       = client_id,
        version   = 2,
        aad_host  = authority
      )
    }
    creds <- env$token$credentials
    c(
      Authorization = paste(creds$token_type, creds$access_token),
      "User-Agent"  = "idata-script-client"
    )
  }
})
