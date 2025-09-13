#-------------------------------------------------------------------------------
# IMF cache maintenance helpers
#-------------------------------------------------------------------------------

# Return (create if needed) a cache env by name
.ensure_cache_env <- function(name) {
  if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    assign(name, new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(name, envir = .GlobalEnv, inherits = FALSE)
}

# Clear BOTH caches entirely
clear_imf_caches <- function() {
  # raw DSD cache
  if (exists(".dsdcache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = ls(.dsdcache, all.names = TRUE), envir = .dsdcache)
  }
  # codelist tibble cache
  if (exists(".codelistcache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = ls(.codelistcache, all.names = TRUE), envir = .codelistcache)
  }
  invisible(TRUE)
}

# Drop a single DSD from the raw DSD cache
drop_dsd_cache <- function(dsd_id) {
  dsd_cache <- .ensure_cache_env(".dsdcache")
  if (exists(dsd_id, envir = dsd_cache, inherits = FALSE)) {
    rm(list = dsd_id, envir = dsd_cache)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Drop codelist tibble cache entries for a given DSD and (optionally) language
# Keys are stored as "<DSD_ID>::<language>"
drop_codelist_cache <- function(dsd_id, language = NULL) {
  cl_cache <- .ensure_cache_env(".codelistcache")
  keys <- ls(cl_cache, all.names = TRUE)
  sel <- if (is.null(language)) {
    startsWith(keys, paste0(dsd_id, "::"))
  } else {
    keys == paste0(dsd_id, "::", language)
  }
  if (any(sel)) rm(list = keys[sel], envir = cl_cache)
  invisible(sum(sel))
}

# Quick introspection helpers (optional)
list_dsd_cache_keys <- function() {
  if (!exists(".dsdcache", envir = .GlobalEnv, inherits = FALSE)) return(character())
  ls(.dsdcache, all.names = TRUE)
}

list_codelist_cache_keys <- function() {
  if (!exists(".codelistcache", envir = .GlobalEnv, inherits = FALSE)) return(character())
  ls(.codelistcache, all.names = TRUE)
}
