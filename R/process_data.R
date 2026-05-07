# ══════════════════════════════════════════════════════════════════
#  process_data.R  –  Plain script, sourced once from dashboard.qmd
#  Produces directly in the R environment:
#    mse_q, sp_q, master          ← cleaned raw data
#    per_mse, per_sp              ← per-MSE completion summaries
#    daily                        ← daily progress
#    team_sum, team6_sum          ← team-level summaries
#    enum_sum                     ← enumerator summary
#    arm_sum                      ← treatment arm summary
#    woreda_cov                   ← woreda coverage
#    sp_by_sparm, sp_by_edtarm    ← spousal sub-arms
#    fu_mse_tbl, fu_sp_tbl        ← follow-up tables
#    rce_enum, rce_block          ← RCE quality checks
#    k                            ← KPI list (scalars for value boxes)
#    D_js                         ← JSON string (JS browser tables only)
# ══════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
  library(purrr)
})

# ── Constants ──────────────────────────────────────────────────────
MSE_TARGET <- 2160L
SP_TARGET  <-  480L
FIELD_DAYS <-   29L

# ── Lookup tables ──────────────────────────────────────────────────
treat_map <- c(
  TechLed  = "Tech-based Training",
  HumanLed = "Human-led Training",
  Control  = "Control"
)

WOREDA_TEAM <- c(
  shabeley="Team A", Goljano="Team A", Mula="Team A", Harawa="Team A",
  Hadhagala="Team A", Gablalu="Team A", Danbal="Team A",
  Degahmadaw="Team B", Garbo="Team B", Duhun="Team B", Ayun="Team B",
  Hararey="Team B",
  Dig="Team C", Daror="Team C", marsin="Team C", Elogaden="Team C",
  Galhamur="Team C", Danot="Team C", Bilcilbur="Team C",
  Adadle="Team D", Denan="Team D", Elele="Team D", Barey="Team D",
  Dolobay="Team D", Godgod="Team D",
  Filtu="Team E", Dekasuftu="Team E", Guradamole="Team E", Kohle="Team E",
  Mubarak="Team E",
  Lagahida="Team F", Salahad="Team F", Qubi="Team F", Mayumuluko="Team F",
  Yahob="Team F", Jarati="Team F"
)

WOREDA_ZONE <- c(
  Adadle="Shebele", Denan="Shebele", Elele="Shebele",
  Ayun="Nogob", Duhun="Nogob", Garbo="Nogob", Hararey="Nogob", Degahmadaw="Nogob",
  Barey="Afder", Dolobay="Afder", Godgod="Afder", Jarati="Afder", Kohle="Afder",
  Bilcilbur="Jerer", Daror="Jerer", Dig="Jerer", Danot="Jerer",
  Lagahida="Erer", Mayumuluko="Erer", Qubi="Erer", Salahad="Erer", Yahob="Erer",
  Goljano="Fafan", Harawa="Fafan", Mula="Fafan", shabeley="Fafan",
  Elogaden="Korahe", marsin="Korahe",
  Dekasuftu="Liben", Filtu="Liben", Guradamole="Liben",
  Mubarak="Dawa", Danbal="Siti", Gablalu="Siti", Hadhagala="Siti",
  Galhamur="Dollo"
)

TEAM_LABELS <- c(
  Daniel="Daniel (A+B)", Anteneh="Anteneh (C+D)",
  Wesen="Wosen (E)",     Muktar="Muktar (F)"
)

# ── Build woreda metadata table (one join, no enframe needed) ──────
woreda_meta <- data.frame(
  woreda = names(WOREDA_TEAM),
  team   = unname(WOREDA_TEAM),
  zone   = unname(WOREDA_ZONE[names(WOREDA_TEAM)]),
  stringsAsFactors = FALSE
)

team_labels_df <- data.frame(
  sup   = names(TEAM_LABELS),
  label = unname(TEAM_LABELS),
  stringsAsFactors = FALSE
)

# ── Helper: parse mixed-format submission dates ────────────────────
parse_date <- function(x) {
  as.Date(parse_date_time(x,
    orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM"),
    quiet  = TRUE))
}

# ══════════════════════════════════════════════════════════════════
#  1. READ RAW CSVs
# ══════════════════════════════════════════════════════════════════
data_dir <- "data"

mse_q   <- read.csv(file.path(data_dir, "MSE questionnaire_WIDE.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)
sp_q    <- read.csv(file.path(data_dir, "Spouse questionnaire_WIDE.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)
master  <- read.csv(file.path(data_dir, "mse_master.csv"),
                    stringsAsFactors = FALSE)

# ══════════════════════════════════════════════════════════════════
#  2. ENRICH MASTER
# ══════════════════════════════════════════════════════════════════
master <- master %>%
  left_join(woreda_meta, by = "woreda") %>%
  mutate(
    team        = if_else(is.na(team), "TBD", team),
    zone        = if_else(is.na(zone), "",    zone),
    treat_label = treat_map[treat]
  )

# Slim lookup for joining onto questionnaire rows
mse_lookup <- master %>%
  select(mse_id, woreda, zone, team, treat_label) %>%
  distinct()

# ══════════════════════════════════════════════════════════════════
#  3. CLEAN MSE QUESTIONNAIRE
# ══════════════════════════════════════════════════════════════════
mse_q <- mse_q %>%
  filter(!is.na(l02_mse_id), trimws(as.character(l02_mse_id)) != "") %>%
  mutate(
    mse_id        = trimws(as.character(l02_mse_id)),
    treatment_arm = treat_map[as.character(calc_treat)],
    treatment_arm = if_else(is.na(treatment_arm), "Unknown", treatment_arm),
    date_only     = parse_date(submissiondate)
  )

# ══════════════════════════════════════════════════════════════════
#  4. CLEAN SPOUSE QUESTIONNAIRE
# ══════════════════════════════════════════════════════════════════
sp_q <- sp_q %>%
  filter(!is.na(L02_mse_id), trimws(as.character(L02_mse_id)) != "") %>%
  mutate(
    mse_id    = trimws(as.character(L02_mse_id)),
    date_only = parse_date(SubmissionDate)
  )

# ══════════════════════════════════════════════════════════════════
#  5. PER-MSE COMPLETION SUMMARIES
# ══════════════════════════════════════════════════════════════════
per_mse <- mse_q %>%
  group_by(mse_id) %>%
  summarise(
    interviews    = n(),
    supervisor    = first(supervisor),
    enumerator    = first(enumerator),
    treatment_arm = first(treatment_arm),
    date_last     = max(date_only, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mse_lookup, by = "mse_id") %>%
  mutate(
    woreda     = if_else(is.na(woreda), "Unknown", woreda),
    team       = if_else(is.na(team),   "Unknown", team),
    interviews = as.integer(interviews),
    missing    = pmax(6L - interviews, 0L),
    complete   = interviews >= 6L,
    followup   = missing > 0L
  )

per_sp <- sp_q %>%
  group_by(mse_id) %>%
  summarise(
    sp_interviews = n(),
    supervisor    = first(supervisor),
    date_last     = max(date_only, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mse_lookup, by = "mse_id") %>%
  mutate(
    woreda        = if_else(is.na(woreda), "Unknown", woreda),
    sp_interviews = as.integer(sp_interviews),
    sp_missing    = pmax(2L - sp_interviews, 0L),
    sp_complete   = sp_interviews >= 2L,
    sp_followup   = sp_missing > 0L
  )

# ══════════════════════════════════════════════════════════════════
#  6. DAILY PROGRESS
# ══════════════════════════════════════════════════════════════════
daily_mse <- mse_q %>%
  filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>%
  summarise(mse_i = n(), mses = n_distinct(mse_id), .groups = "drop")

daily_sp <- sp_q %>%
  filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>%
  summarise(sp_i = n(), .groups = "drop")

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

# ══════════════════════════════════════════════════════════════════
#  7. TREATMENT ARM SUMMARY
# ══════════════════════════════════════════════════════════════════
arm_sum <- mse_q %>%
  filter(treatment_arm != "Unknown") %>%
  group_by(a = treatment_arm) %>%
  summarise(i = n(), m = n_distinct(mse_id), .groups = "drop") %>%
  mutate(t = 720L, p = round(i / t * 100, 1))

# ══════════════════════════════════════════════════════════════════
#  8. TEAM SUMMARIES  (supervisor-level and Team A–F level)
# ══════════════════════════════════════════════════════════════════
mse_by_sup <- mse_q %>%
  group_by(sup = supervisor) %>%
  summarise(i = n(), m = n_distinct(mse_id), .groups = "drop")

sp_by_sup <- sp_q %>%
  group_by(sup = supervisor) %>%
  summarise(si = n(), sm = n_distinct(mse_id), .groups = "drop")

fu_mse_by_sup <- per_mse %>% filter(followup)    %>% count(sup = supervisor, name = "fu_mse")
fu_sp_by_sup  <- per_sp  %>% filter(sp_followup) %>% count(sup = supervisor, name = "fu_sp")

team_sum <- mse_by_sup %>%
  left_join(sp_by_sup,     by = "sup") %>%
  left_join(fu_mse_by_sup, by = "sup") %>%
  left_join(fu_sp_by_sup,  by = "sup") %>%
  replace_na(list(si = 0L, sm = 0L, fu_mse = 0L, fu_sp = 0L)) %>%
  left_join(team_labels_df, by = "sup") %>%
  mutate(label = if_else(is.na(label), sup, label))

# Team A–F level
mse_by_team <- mse_q %>%
  left_join(mse_lookup %>% select(mse_id, team), by = "mse_id") %>%
  mutate(team = if_else(is.na(team), "TBD", team)) %>%
  group_by(team) %>%
  summarise(i = n(), m = n_distinct(mse_id), .groups = "drop")

sp_by_team <- sp_q %>%
  left_join(mse_lookup %>% select(mse_id, team), by = "mse_id") %>%
  mutate(team = if_else(is.na(team), "TBD", team)) %>%
  group_by(team) %>%
  summarise(si = n(), sm = n_distinct(mse_id), .groups = "drop")

comp_by_team    <- per_mse %>% group_by(team) %>%
  summarise(comp = sum(complete), incomp = sum(followup), .groups = "drop")
sp_comp_by_team <- per_sp  %>% group_by(team) %>%
  summarise(sp_comp = sum(sp_complete), sp_incomp = sum(sp_followup), .groups = "drop")
fu_mse_by_team  <- per_mse %>% filter(followup)    %>% count(team, name = "fu_mse")
fu_sp_by_team   <- per_sp  %>% filter(sp_followup) %>% count(team, name = "fu_sp")

team6_sum <- tibble(team = c("Team A","Team B","Team C","Team D","Team E","Team F")) %>%
  left_join(mse_by_team,     by = "team") %>%
  left_join(sp_by_team,      by = "team") %>%
  left_join(comp_by_team,    by = "team") %>%
  left_join(sp_comp_by_team, by = "team") %>%
  left_join(fu_mse_by_team,  by = "team") %>%
  left_join(fu_sp_by_team,   by = "team") %>%
  replace_na(list(i=0L,m=0L,si=0L,sm=0L,comp=0L,incomp=0L,
                  sp_comp=0L,sp_incomp=0L,fu_mse=0L,fu_sp=0L))

# ══════════════════════════════════════════════════════════════════
#  9. ENUMERATOR SUMMARY
# ══════════════════════════════════════════════════════════════════
enum_sum <- mse_q %>%
  group_by(sup = supervisor, e = enumerator) %>%
  summarise(i = n(), m = n_distinct(mse_id), .groups = "drop") %>%
  arrange(sup, desc(i))

# ══════════════════════════════════════════════════════════════════
#  10. WOREDA COVERAGE
# ══════════════════════════════════════════════════════════════════
interviews_by_woreda <- per_mse %>%
  group_by(woreda) %>%
  summarise(i = sum(interviews), ms = n(), .groups = "drop")

woreda_cov <- master %>%
  group_by(woreda) %>%
  summarise(tgt = n(), zone = first(zone), team = first(team), .groups = "drop") %>%
  mutate(it = tgt * 6L) %>%
  left_join(interviews_by_woreda, by = "woreda") %>%
  replace_na(list(i = 0L, ms = 0L)) %>%
  mutate(
    p = round(i / it * 100, 1),
    s = case_when(
      p >= 100 ~ "Complete",
      p >    0 ~ "In Progress",
      TRUE     ~ "Not Started"
    )
  ) %>%
  rename(w = woreda, z = zone, t = team)

# ══════════════════════════════════════════════════════════════════
#  11. SPOUSAL SUPPORT & EDUTAINMENT STATUS
# ══════════════════════════════════════════════════════════════════
sp_support_master <- master %>%
  mutate(
    sp_arm  = case_when(
      trimws(spousal_support) == "SpousalSuport" ~ "Spousal Support",
      trimws(spousal_support) == "SPControl"     ~ "SP Control",
      TRUE ~ NA_character_
    ),
    edt_arm = case_when(
      trimws(edutainment) == "Edutainment" ~ "Edutainment",
      trimws(edutainment) == "EDTControl"  ~ "EDT Control",
      TRUE ~ NA_character_
    )
  )

sp_done <- sp_q %>%
  group_by(mse_id) %>%
  summarise(sp_done = n(), .groups = "drop")

sp_support_status <- sp_support_master %>%
  left_join(sp_done, by = "mse_id") %>%
  mutate(sp_done = replace_na(sp_done, 0L), sp_complete = sp_done >= 2L)

sp_by_sparm <- sp_support_status %>%
  filter(!is.na(sp_arm)) %>%
  group_by(arm = sp_arm) %>%
  summarise(
    eligible    = n(),
    completed   = sum(sp_complete),
    started     = sum(sp_done > 0L & !sp_complete),
    not_started = sum(sp_done == 0L),
    .groups = "drop"
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
    .groups = "drop"
  ) %>%
  mutate(pct = round(completed / eligible * 100, 1))

# ══════════════════════════════════════════════════════════════════
#  12. FOLLOW-UP TABLES
# ══════════════════════════════════════════════════════════════════
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
    dl   = format(date_last, "%d-%b")
  )

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

# ══════════════════════════════════════════════════════════════════
#  13. RCE QUALITY CHECK
# ══════════════════════════════════════════════════════════════════
RCE_BLOCKS <- list(
  list(block=1L, qs=c("choice_set_2","choice_set_6","choice_set_11",
                      "choice_set_21","choice_set_22","choice_set_24"),
       orig="choice_set_6",   repeat_q="choice_set_6_2"),
  list(block=2L, qs=c("choice_set_7","choice_set_10","choice_set_13",
                      "choice_set_14","choice_set_19","choice_set_20"),
       orig="choice_set_10",  repeat_q="choice_set_10_2"),
  list(block=3L, qs=c("choice_set_8","choice_set_9","choice_set_15",
                      "choice_set_16","choice_set_17","choice_set_18"),
       orig="choice_set_9",   repeat_q="choice_set_9_2"),
  list(block=4L, qs=c("choice_set_1","choice_set_3","choice_set_4",
                      "choice_set_5","choice_set_12","choice_set_23"),
       orig="choice_set_3",   repeat_q="choice_set_3_2")
)

all_rce_cols <- unique(c(
  unlist(lapply(RCE_BLOCKS, `[[`, "qs")),
  unlist(lapply(RCE_BLOCKS, `[[`, "orig")),
  unlist(lapply(RCE_BLOCKS, `[[`, "repeat_q"))
))
missing_rce <- setdiff(all_rce_cols, names(mse_q))

if (length(missing_rce) > 0) {
  warning("RCE columns missing - skipping RCE tab: ", paste(missing_rce, collapse = ", "))
  rce_enum  <- data.frame()
  rce_block <- data.frame()
} else {

  # Mismatch at respondent level (before any pivot)
  rce_mismatch <- bind_rows(lapply(RCE_BLOCKS, function(blk) {
    mse_q %>%
      select(enumerator, supervisor,
             orig_ans   = all_of(blk$orig),
             repeat_ans = all_of(blk$repeat_q)) %>%
      mutate(
        block      = blk$block,
        orig_ans   = toupper(trimws(as.character(orig_ans))),
        repeat_ans = toupper(trimws(as.character(repeat_ans))),
        mismatch   = case_when(
          is.na(orig_ans)   | orig_ans   == "" ~ NA,
          is.na(repeat_ans) | repeat_ans == "" ~ NA,
          TRUE ~ orig_ans != repeat_ans
        )
      ) %>%
      select(enumerator, supervisor, block, mismatch)
  }))

  mismatch_by_enum  <- rce_mismatch %>%
    group_by(enumerator, supervisor, block) %>%
    summarise(mismatch = sum(mismatch, na.rm = TRUE), .groups = "drop")

  mismatch_by_block <- rce_mismatch %>%
    group_by(block) %>%
    summarise(mismatches = sum(mismatch, na.rm = TRUE), .groups = "drop")

  # A/B/C frequency (pivot_longer per block)
  rce_long <- bind_rows(lapply(RCE_BLOCKS, function(blk) {
    mse_q %>%
      select(enumerator, supervisor, all_of(blk$qs)) %>%
      mutate(block = blk$block) %>%
      pivot_longer(cols = all_of(blk$qs), names_to = "question", values_to = "response") %>%
      mutate(response = toupper(trimws(as.character(response)))) %>%
      select(enumerator, supervisor, block, question, response)
  }))

  rce_enum <- rce_long %>%
    filter(!is.na(response), response %in% c("A","B","C")) %>%
    group_by(enumerator, supervisor, block) %>%
    summarise(
      n_resp = n(),
      freq_A = sum(response == "A"),
      freq_B = sum(response == "B"),
      freq_C = sum(response == "C"),
      .groups = "drop"
    ) %>%
    left_join(mismatch_by_enum, by = c("enumerator","supervisor","block")) %>%
    replace_na(list(mismatch = 0L)) %>%
    mutate(
      pct_A        = round(freq_A / n_resp * 100, 1),
      pct_B        = round(freq_B / n_resp * 100, 1),
      pct_C        = round(freq_C / n_resp * 100, 1),
      max_pct      = pmax(pct_A, pct_B, pct_C),
      flag_uniform = max_pct >= 80
    ) %>%
    arrange(block, desc(flag_uniform), desc(mismatch), enumerator)

  rce_block <- rce_long %>%
    filter(!is.na(response), response %in% c("A","B","C")) %>%
    group_by(block) %>%
    summarise(
      n_resp = n(),
      freq_A = sum(response == "A"),
      freq_B = sum(response == "B"),
      freq_C = sum(response == "C"),
      .groups = "drop"
    ) %>%
    left_join(mismatch_by_block, by = "block") %>%
    replace_na(list(mismatches = 0L)) %>%
    mutate(
      pct_A = round(freq_A / n_resp * 100, 1),
      pct_B = round(freq_B / n_resp * 100, 1),
      pct_C = round(freq_C / n_resp * 100, 1)
    )
}

# ══════════════════════════════════════════════════════════════════
#  14. KPIs  (plain list of scalars for value boxes)
# ══════════════════════════════════════════════════════════════════
k <- list(
  data_date      = format(max(mse_q$date_only, na.rm = TRUE), "%d-%b-%Y"),
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
  n_days         = as.integer(max(mse_q$date_only, na.rm=TRUE) -
                               min(mse_q$date_only, na.rm=TRUE)) + 1L,
  date_start     = format(min(mse_q$date_only, na.rm = TRUE), "%d-%b"),
  date_end       = format(max(mse_q$date_only, na.rm = TRUE), "%d-%b"),
  woredas_active = sum(woreda_cov$ms > 0)
)

# ══════════════════════════════════════════════════════════════════
#  15. JSON  –  only for browser-side JS table rendering
#  All R charts use the data frames above directly (no fromJSON)
# ══════════════════════════════════════════════════════════════════
D_js <- toJSON(list(
  daily     = as.data.frame(daily %>%
                select(date = date_label, mse_i, sp_i, cum_mse, cum_sp, mses, mse_pace, sp_pace)),
  teams     = as.data.frame(team_sum),
  team6     = as.data.frame(team6_sum),
  enums     = as.data.frame(enum_sum),
  arm       = as.data.frame(arm_sum),
  fu_mse    = as.data.frame(fu_mse_tbl),
  fu_sp     = as.data.frame(fu_sp_tbl),
  woreda    = as.data.frame(woreda_cov %>% select(z, w, t, tgt, it, i, ms, p, s)),
  sp_sparm  = as.data.frame(sp_by_sparm),
  sp_edtarm = as.data.frame(sp_by_edtarm),
  rce_enum  = as.data.frame(rce_enum),
  rce_block = as.data.frame(rce_block)
), auto_unbox = TRUE, na = "null")
