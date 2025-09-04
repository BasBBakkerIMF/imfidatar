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


#’ -----------------------------------------------------------------------------
#’ Internal helper: build HTTP headers for IMF API calls
#’ -----------------------------------------------------------------------------
.get_imf_headers <- local({
  env <- new.env(parent = emptyenv())
  client_id <- Sys.getenv("IMFIDATA_CLIENT_ID", "446ce2fa-88b1-436c-b8e6-94491ca4f6fb")

  # B2C host (scheme + trailing slash) and tenant INCLUDING POLICY
  aad_host <- Sys.getenv("IMFIDATA_AAD_HOST", "https://imfprdb2c.b2clogin.com/")
  tenant   <- Sys.getenv("IMFIDATA_TENANT_WITH_POLICY",
                         "imfprdb2c.onmicrosoft.com/b2c_1a_signin_aad_simple_user_journey")

  # Delegated resource (single line)
  resource <- Sys.getenv("IMFIDATA_SCOPE",
                         "https://imfprdb2c.onmicrosoft.com/4042e178-3e2f-4ff9-ac38-1276c901c13d/iData.Login")

  # Add offline_access so the server returns a refresh_token
  scope_str <- paste(resource, "offline_access")

  function(needs_auth = TRUE) {
    if (identical(needs_auth, FALSE)) {
      return(c("User-Agent" = "imfidata-client"))
    }

    # acquire or refresh token
    if (!exists("token", envir = env) ||
        !isTRUE(tryCatch(env$token$validate(), error = function(e) FALSE))) {
      if (exists("token", envir = env)) {
        try(env$token$refresh(), silent = TRUE)
      }
      if (!exists("token", envir = env) ||
          !isTRUE(tryCatch(env$token$validate(), error = function(e) FALSE))) {
        env$token <- AzureAuth::get_azure_token(
          resource = resource,   # Option A
          tenant   = tenant,     # includes policy segment
          app      = client_id,
          version  = 2,
          aad_host = aad_host,
          # ensure offline_access is requested (and match MSAL localhost redirect)
          authorize_args = list(scope = scope_str, redirect_uri = "http://localhost:1410/"),
          token_args      = list(scope = scope_str)
        )
      }
    }

    creds <- env$token$credentials
    c(
      Authorization = paste0(creds$token_type, " ", creds$access_token),
      "User-Agent"  = "imfidata-client"
    )
  }
})

