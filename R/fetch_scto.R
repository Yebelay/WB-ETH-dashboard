# R/fetch_scto.R
# Live data pull from SurveyCTO — AFRGI server
# Forms: mse_member_listing_v2, mse_questionnaire_v2, spouse_questionnaire_v2

library(httr)
library(jsonlite)
library(dplyr)
library(glue)

# ── Credentials (read from .Renviron — never hardcode in shared scripts) ──
SCTO_SERVER   <- Sys.getenv("SCTO_SERVER",   "afrgi")
SCTO_USER     <- Sys.getenv("SCTO_USER",     "yebelay.berehan@c4ed.org")
SCTO_PASSWORD <- Sys.getenv("SCTO_PASSWORD", "LLRPMSE12!@")

# ── Base fetcher ──
.scto_get <- function(form_id, repeat_group = NULL, date_from = NULL) {
  base <- glue("https://{SCTO_SERVER}.surveycto.com/api/v2/forms/data/wide/json/{form_id}")
  url  <- if (!is.null(repeat_group)) paste0(base, "/", repeat_group) else base
  
  query <- if (!is.null(date_from)) list(date = date_from) else list()
  
  resp <- GET(
    url,
    authenticate(SCTO_USER, SCTO_PASSWORD, type = "basic"),
    query   = query,
    timeout(120)
  )
  
  if (status_code(resp) != 200) {
    stop(glue(
      "SurveyCTO API error [{status_code(resp)}] for form '{form_id}': ",
      content(resp, "text", encoding = "UTF-8")
    ))
  }
  
  raw <- content(resp, "text", encoding = "UTF-8")
  
  # empty response guard
  if (nchar(trimws(raw)) == 0 || raw == "[]") {
    warning(glue("No data returned for form '{form_id}'"))
    return(tibble())
  }
  
  fromJSON(raw, flatten = TRUE) |> as_tibble()
}

# ── Cached fetcher ──
.scto_cached <- function(form_id,
                         repeat_group  = NULL,
                         cache_file    = NULL,
                         max_age_min   = 30,
                         date_from     = NULL) {
  
  if (is.null(cache_file))
    cache_file <- glue("data/cache_{form_id}{if(!is.null(repeat_group)) paste0('_',repeat_group) else ''}.rds")
  
  dir.create("data", showWarnings = FALSE)
  
  if (file.exists(cache_file)) {
    age_min <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "mins"))
    if (age_min < max_age_min) {
      message(glue("  [cache] {basename(cache_file)} ({round(age_min,1)} min old) — skipping API call"))
      return(readRDS(cache_file))
    }
  }
  
  message(glue("  [fetch] Pulling '{form_id}' from SurveyCTO..."))
  dat <- .scto_get(form_id, repeat_group, date_from)
  saveRDS(dat, cache_file)
  message(glue("  [fetch] {nrow(dat)} rows saved to {cache_file}"))
  dat
}

# ── Public functions — call these from process_data.R ──

# 1. MSE Member Listing
fetch_member_listing <- function(max_age_min = 30, date_from = NULL) {
  .scto_cached(
    form_id     = "mse_member_listing_v2",
    cache_file  = "data/cache_member_listing.rds",
    max_age_min = max_age_min,
    date_from   = date_from
  )
}

# 2. MSE Questionnaire (main survey)
fetch_mse <- function(max_age_min = 30, date_from = NULL) {
  .scto_cached(
    form_id     = "mse_questionnaire_v2",
    cache_file  = "data/cache_mse.rds",
    max_age_min = max_age_min,
    date_from   = date_from
  )
}

# 3. Spouse Questionnaire
fetch_spouse <- function(max_age_min = 30, date_from = NULL) {
  .scto_cached(
    form_id     = "spouse_questionnaire_v2",
    cache_file  = "data/cache_spouse.rds",
    max_age_min = max_age_min,
    date_from   = date_from
  )
}

# 4. Any repeat group inside a form (generic)
fetch_repeat <- function(form_id, repeat_group, max_age_min = 30) {
  .scto_cached(
    form_id      = form_id,
    repeat_group = repeat_group,
    max_age_min  = max_age_min
  )
}

# ── Connection test — run interactively to verify credentials ──
test_scto_connection <- function() {
  message("Testing SurveyCTO connection...")
  tryCatch({
    d <- .scto_get("mse_questionnaire_v2")
    message(glue("✅ Connected — {nrow(d)} MSE rows returned"))
    invisible(d)
  }, error = function(e) {
    message("❌ Connection failed: ", e$message)
    invisible(NULL)
  })
}
