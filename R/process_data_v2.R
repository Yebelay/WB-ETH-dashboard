suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
})

# ── Lookup tables ─────────────────────────────────────────
WOREDA_TEAM <- c(
  shabeley   = "Team A", Goljano  = "Team A", Mula      = "Team A", Harawa    = "Team A",
  Hadhagala  = "Team A", Gablalu  = "Team A", Danbal    = "Team A",
  Degahmadaw = "Team B", Garbo    = "Team B", Duhun     = "Team B", Ayun      = "Team B",
  Hararey    = "Team B",
  Dig        = "Team C", Daror    = "Team C", marsin    = "Team C", Elogaden  = "Team C",
  Galhamur   = "Team C", Danot    = "Team C", Bilcilbur = "Team C",
  Adadle     = "Team D", Denan    = "Team D", Elele     = "Team D", Barey     = "Team D",
  Dolobay    = "Team D", Godgod   = "Team D",
  Filtu      = "Team E", Dekasuftu = "Team E", Guradamole = "Team E", Kohle   = "Team E",
  Mubarak    = "Team E",
  Lagahida   = "Team F", Salahad  = "Team F", Qubi      = "Team F", Mayumuluko = "Team F",
  Yahob      = "Team F", Jarati   = "Team F")

WOREDA_ZONE <- c(
  Adadle     = "Shebele", Denan      = "Shebele", Elele      = "Shebele",
  Ayun       = "Nogob",   Duhun      = "Nogob",   Garbo      = "Nogob",
  Hararey    = "Nogob",   Degahmadaw = "Nogob",
  Barey      = "Afder",   Dolobay    = "Afder",   Godgod     = "Afder",
  Jarati     = "Afder",   Kohle      = "Afder",
  Bilcilbur  = "Jerer",   Daror      = "Jerer",   Dig        = "Jerer",   Danot  = "Jerer",
  Lagahida   = "Erer",    Mayumuluko = "Erer",    Qubi       = "Erer",
  Salahad    = "Erer",    Yahob      = "Erer",
  Goljano    = "Fafan",   Harawa     = "Fafan",   Mula       = "Fafan",   shabeley = "Fafan",
  Elogaden   = "Korahe",  marsin     = "Korahe",
  Dekasuftu  = "Liben",   Filtu      = "Liben",   Guradamole = "Liben",
  Mubarak    = "Dawa",    Danbal     = "Siti",    Gablalu    = "Siti",
  Hadhagala  = "Siti",    Galhamur   = "Dollo"
)

TREAT_LABELS <- c(
  TechLed  = "Tech-based Training",
  HumanLed = "Human-led Training",
  Control  = "Control"
)

TEAM_LABELS <- c(
  Daniel  = "Daniel (A+B)",
  Anteneh = "Anteneh (C+D)",
  Wesen   = "Wosen (E)",
  Muktar  = "Muktar (F)"
)

MSE_TARGET <- 2160L
SP_TARGET  <- 480L
FIELD_DAYS <- 29L

# ── Helper: convert lookup vectors to join-ready tibbles ──
woreda_team_tbl   <- enframe(WOREDA_TEAM, name = "woreda", value = "team")
woreda_zone_tbl   <- enframe(WOREDA_ZONE, name = "woreda", value = "zone")
team_labels_tbl   <- enframe(TEAM_LABELS, name = "sup",    value = "label")

# ── Helper: parse submission date (used in both mse_q and sp_q) ──
parse_submission_date <- function(x) {
  as.Date(
    parse_date_time(
      x,
      orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM"),
      quiet  = TRUE
    )
  )
}

# ── Main Processing Function ──────────────────────────────
load_and_process <- function(data_dir = "data") {
  
  files <- c(
    mse     = "MSE questionnaire_WIDE.csv",
    spouse  = "Spouse questionnaire_WIDE.csv",
    master  = "mse_master.csv",
    listing = "MSE Member Listing_WIDE.csv"
  )
  
  # Check all files exist before reading
  for (f in files) {
    target_path <- file.path(data_dir, f)
    if (!file.exists(target_path)) {
      stop(paste0("CRITICAL ERROR: File not found at: ", target_path))
    }
  }
  
  # ── Read CSVs ──────────────────────────────────────────
  mse_q   <- read.csv(file.path(data_dir, files["mse"]),     stringsAsFactors = FALSE, check.names = FALSE)
  sp_q    <- read.csv(file.path(data_dir, files["spouse"]),  stringsAsFactors = FALSE, check.names = FALSE)
  master  <- read.csv(file.path(data_dir, files["master"]),  stringsAsFactors = FALSE)
  listing <- read.csv(file.path(data_dir, files["listing"]), stringsAsFactors = FALSE, check.names = FALSE)
  
  # ── Enrich master with zone, team, and treatment label ─
  master <- master %>%
    left_join(woreda_team_tbl, by = "woreda") %>%
    left_join(woreda_zone_tbl, by = "woreda") %>%
    mutate(
      team        = replace_na(team, "TBD"),
      zone        = replace_na(zone, ""),
      treat_label = case_match(
        treat,
        "TechLed"  ~ "Tech-based Training",
        "HumanLed" ~ "Human-led Training",
        "Control"  ~ "Control",
        .default   = treat
      )
    )
  
  # Slim lookup for joining onto questionnaire data
  mse_lookup <- master %>%
    select(mse_id, woreda, zone, team, treat_label) %>%
    distinct()
  
  # ── Clean MSE questionnaire ────────────────────────────
  mse_q <- mse_q %>%
    filter(!is.na(l02_mse_id), l02_mse_id != "") %>%
    mutate(
      mse_id        = str_trim(as.character(l02_mse_id)),
      treatment_arm = case_match(
        as.character(calc_treat),
        "TechLed"  ~ "Tech-based Training",
        "HumanLed" ~ "Human-led Training",
        "Control"  ~ "Control",
        .default   = "Unknown"
      ),
      date_only = parse_submission_date(submissiondate)
    )
  
  # ── Clean spouse questionnaire ─────────────────────────
  sp_q <- sp_q %>%
    filter(!is.na(L02_mse_id), L02_mse_id != "") %>%
    mutate(
      mse_id    = str_trim(as.character(L02_mse_id)),
      date_only = parse_submission_date(SubmissionDate))
  
  # ── Per-MSE completion summary ─────────────────────────
  per_mse <- mse_q %>%
    group_by(mse_id) %>%
    summarise(
      interviews    = n(),
      supervisor    = first(supervisor),
      enumerator    = first(enumerator),
      treatment_arm = first(treatment_arm),
      date_last     = max(date_only, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    left_join(mse_lookup, by = "mse_id") %>%
    mutate(
      woreda     = replace_na(woreda, "Unknown"),
      team       = replace_na(team,   "Unknown"),
      interviews = as.integer(interviews),
      missing    = pmax(6L - interviews, 0L),
      complete   = interviews >= 6L,
      followup   = missing > 0L
    )
  
  # ── Per-MSE spouse summary ─────────────────────────────
  per_sp <- sp_q %>%
    group_by(mse_id) %>%
    summarise(
      sp_interviews = n(),
      supervisor    = first(supervisor),
      date_last     = max(date_only, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    left_join(mse_lookup, by = "mse_id") %>%
    mutate(
      woreda        = replace_na(woreda, "Unknown"),
      sp_interviews = as.integer(sp_interviews),
      sp_missing    = pmax(2L - sp_interviews, 0L),
      sp_complete   = sp_interviews >= 2L,
      sp_followup   = sp_missing > 0L
    )
  
  # ── Daily progress ─────────────────────────────────────
  daily_mse <- mse_q %>%
    filter(!is.na(date_only)) %>%
    group_by(date = date_only) %>%
    summarise(
      mse_i = n(),
      mses  = n_distinct(mse_id),
      .groups = "drop"
    )
  
  daily_sp <- sp_q %>%
    filter(!is.na(date_only)) %>%
    group_by(date = date_only) %>%
    summarise(
      sp_i = n(),
      .groups = "drop"
    )
  
  daily <- full_join(daily_mse, daily_sp, by = "date") %>%
    replace_na(list(mse_i = 0L, mses = 0L, sp_i = 0L)) %>%
    arrange(date) %>%
    mutate(
      cum_mse    = cumsum(mse_i),
      cum_sp     = cumsum(sp_i),
      date_label = format(date, "%d-%b"),
      day_num    = as.integer(date - min(date, na.rm = TRUE)) + 1L,
      mse_pace   = round(MSE_TARGET / FIELD_DAYS * day_num),
      sp_pace    = round(SP_TARGET  / FIELD_DAYS * day_num)
    )
  
  # ── Treatment arm summary ──────────────────────────────
  arm_sum <- mse_q %>%
    filter(treatment_arm != "Unknown") %>%
    group_by(a = treatment_arm) %>%
    summarise(
      i = n(),
      m = n_distinct(mse_id),
      .groups = "drop"
    ) %>%
    mutate(
      t = 720L,
      p = round(i / t * 100, 1)
    )
  
  # ── Team summary (split into named steps for clarity) ──
  
  mse_by_sup <- mse_q %>%
    group_by(sup = supervisor) %>%
    summarise(
      i = n(),
      m = n_distinct(mse_id),
      .groups = "drop"
    )
  
  sp_by_sup <- sp_q %>%
    group_by(sup = supervisor) %>%
    summarise(
      si = n(),
      sm = n_distinct(mse_id),
      .groups = "drop"
    )
  
  fu_mse_by_sup <- per_mse %>%
    filter(followup) %>%
    count(sup = supervisor, name = "fu_mse")
  
  fu_sp_by_sup <- per_sp %>%
    filter(sp_followup) %>%
    count(sup = supervisor, name = "fu_sp")
  
  team_sum <- mse_by_sup %>%
    left_join(sp_by_sup,     by = "sup") %>%
    left_join(fu_mse_by_sup, by = "sup") %>%
    left_join(fu_sp_by_sup,  by = "sup") %>%
    replace_na(list(si = 0L, sm = 0L, fu_mse = 0L, fu_sp = 0L)) %>%
    left_join(team_labels_tbl, by = "sup") %>%
    mutate(label = coalesce(label, sup))
  
  # ── Enumerator summary ─────────────────────────────────
  enum_sum <- mse_q %>%
    group_by(sup = supervisor, e = enumerator) %>%
    summarise(
      i = n(),
      m = n_distinct(mse_id),
      .groups = "drop"
    ) %>%
    arrange(sup, desc(i))
  
  # ── Woreda coverage ────────────────────────────────────
  interviews_by_woreda <- per_mse %>%
    group_by(woreda) %>%
    summarise(
      i  = sum(interviews),
      ms = n(),
      .groups = "drop"
    )
  
  woreda_cov <- master %>%
    group_by(woreda) %>%
    summarise(
      tgt  = n(),
      zone = first(zone),
      team = first(team),
      .groups = "drop"  ) %>%
    mutate(it = tgt * 6L) %>%
    left_join(interviews_by_woreda, by = "woreda") %>%
    replace_na(list(i = 0L, ms = 0L)) %>%
    mutate(
      p = round(i / it * 100, 1),
      s = case_when(
        p >= 100 ~ "Complete",
        p >  0   ~ "In Progress",
        TRUE     ~ "Not Started")) %>%
    rename(w = woreda, z = zone, t = team)
  
  # ── Spousal support & edutainment eligibility from master ──────
  sp_support_master <- master %>%
    mutate(
      sp_arm = case_when(
        str_trim(spousal_support) == "SpousalSuport" ~ "Spousal Support",
        str_trim(spousal_support) == "SPControl"     ~ "SP Control",
        TRUE                                         ~ NA_character_
      ),
      edt_arm = case_when(
        str_trim(edutainment) == "Edutainment"  ~ "Edutainment",
        str_trim(edutainment) == "EDTControl"   ~ "EDT Control",
        TRUE                                    ~ NA_character_
      )
    )

  sp_done <- sp_q %>%
    group_by(mse_id) %>%
    summarise(sp_done = n(), .groups = "drop")

  sp_support_status <- sp_support_master %>%
    left_join(sp_done, by = "mse_id") %>%
    mutate(
      sp_done     = replace_na(sp_done, 0L),
      sp_complete = sp_done >= 2L
    )

  sp_by_sparm <- sp_support_status %>%
    filter(!is.na(sp_arm)) %>%
    group_by(arm = sp_arm) %>%
    summarise(
      eligible    = n(),
      completed   = sum(sp_complete),
      started     = sum(sp_done > 0L & !sp_complete),
      not_started = sum(sp_done == 0L),
      .groups     = "drop"
    ) %>%
    mutate(pct = round(completed / eligible * 100, 1))

  sp_by_edtarm <- sp_support_status %>%
    filter(!is.na(edt_arm)) %>%
    group_by(arm = edt_arm) %>%
    summarise(
      eligible    = n(),
      completed   = sum(sp_complete),
      started     = sum(sp_done > 0L & !sp_complete),
      not_started = sum(sp_done == 0L),
      .groups     = "drop"
    ) %>%
    mutate(pct = round(completed / eligible * 100, 1))

  # ── Follow-up tables ───────────────────────────────────
  fu_mse_tbl <- per_mse %>%
    filter(followup) %>%
    transmute(
      id   = mse_id,
      w    = woreda,
      z    = zone,
      team = team,
      sup  = supervisor,
      arm  = treatment_arm,
      i    = as.integer(interviews),
      miss = as.integer(missing),
      dl   = format(date_last, "%d-%b"))
  
  fu_sp_tbl <- per_sp %>%
    filter(sp_followup) %>%
    transmute(
      id   = mse_id,
      w    = woreda,
      team = team,
      sup  = supervisor,
      arm  = treat_label,
      i    = as.integer(sp_interviews),
      miss = as.integer(sp_missing),
      dl   = format(date_last, "%d-%b")
    )
  
  # ── KPIs ───────────────────────────────────────────────
  data_date <- format(max(mse_q$date_only, na.rm = TRUE), "%d-%b-%Y")
  
  # ── Final output list ──────────────────────────────────
  D_list <- list(
    daily      = as.data.frame(daily %>% select(date = date_label, mse_i, sp_i, cum_mse, cum_sp, mses, mse_pace, sp_pace)),
    teams      = as.data.frame(team_sum),
    team6      = as.data.frame(team6_sum),   # Team A/B/C/D/E/F individually
    enums      = as.data.frame(enum_sum),
    arm        = as.data.frame(arm_sum),
    fu_mse     = as.data.frame(fu_mse_tbl),
    fu_sp      = as.data.frame(fu_sp_tbl),
    woreda     = as.data.frame(woreda_cov %>% select(z, w, t, tgt, it, i, ms, p, s)),
    sp_sparm   = as.data.frame(sp_by_sparm),
    sp_edtarm  = as.data.frame(sp_by_edtarm)
  )
  
  return(list(
    D_json = toJSON(D_list, auto_unbox = TRUE, na = "null"),
    kpis   = list(
      data_date      = data_date,
      mse_i          = nrow(mse_q),
      mse_u          = n_distinct(mse_q$mse_id),
      mse_comp       = sum(per_mse$complete),
      mse_incomp     = sum(per_mse$followup),
      mse_pct        = round(nrow(mse_q) / MSE_TARGET * 100, 1),
      sp_i           = nrow(sp_q),
      sp_u           = n_distinct(sp_q$mse_id),
      sp_comp        = sum(per_sp$sp_complete),
      sp_incomp      = sum(per_sp$sp_followup),
      sp_pct         = round(nrow(sp_q) / SP_TARGET * 100, 1),
      n_days         = as.integer(max(mse_q$date_only, na.rm = TRUE) - min(mse_q$date_only, na.rm = TRUE)) + 1L,
      date_start     = format(min(mse_q$date_only, na.rm = TRUE), "%d-%b"),
      date_end       = format(max(mse_q$date_only, na.rm = TRUE), "%d-%b"),
      woredas_active = sum(woreda_cov$ms > 0)
    )
  ))
}
