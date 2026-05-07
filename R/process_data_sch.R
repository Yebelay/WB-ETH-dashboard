# ══════════════════════════════════════════════════════════════════════════════
#  process_data.R  –  Education Survey Baseline · School Dashboard
#  Plain script, sourced once from dashboard.qmd
#
#  Produces in the R environment:
#    principal_q, teacher_q, student_q, school_info  ← cleaned raw data
#    per_school                                       ← per-school completion summary
#    daily                                            ← daily submission progress
#    region_sum                                       ← regional summary
#    instrument_sum                                   ← instrument-level summary
#    school_status                                    ← per-school status (complete/in progress/not started)
#    fu_tbl                                           ← follow-up table (incomplete schools)
#    enroll_sum                                       ← enrollment summary from SEC2
#    k                                                ← KPI list (scalars for value boxes)
#    D_js                                             ← JSON string (JS browser table rendering)
# ══════════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
  library(purrr)
})

# ── Constants ─────────────────────────────────────────────────────────────────
SCHOOL_TARGET    <- 300L   # total schools in sample
P_TARGET         <- 1L     # min principals per school (at least 1)
T_TARGET         <- 3L     # min teachers per school
S_TARGET         <- 8L     # min students per school
FIELD_DAYS       <- 30L    # total planned field days (3 Dec 2025 – 1 Jan 2026)

# ── Helper: parse mixed-format submission dates ────────────────────────────────
#    SubmissionDate format is consistent across all files: "12/20/2025, 7:23:04 PM"
#    lubridate handles this with mdy_hms; fallback to parse_date_time for safety
parse_date <- function(x) {
  as.Date(parse_date_time(x,
    orders = c("mdy HMSp", "mdy HMS", "ymd HMS", "dmy HMS"),
    quiet  = TRUE))
}

# ══════════════════════════════════════════════════════════════════════════════
#  1. READ RAW CSVs
# ══════════════════════════════════════════════════════════════════════════════
data_dir <- "data"

principal_q  <- read.csv(file.path(data_dir, "School Principal survey_WIDE_keyed.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE)
teacher_q    <- read.csv(file.path(data_dir, "Teachers survey_WIDE_keyed.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE)
student_q    <- read.csv(file.path(data_dir, "Student survey_WIDE_keyed.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE)
school_info  <- read.csv(file.path(data_dir, "School Information Survey_WIDE_keyed.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
#  2. CLEAN PRINCIPAL QUESTIONNAIRE
#     Key columns: code, region, zone, woreda, school_name, rural,
#                  SubmissionDate, respondent_code, sex, role
# ══════════════════════════════════════════════════════════════════════════════
principal_q <- principal_q %>%
  filter(!is.na(code), trimws(as.character(code)) != "") %>%
  mutate(
    school_code   = trimws(as.character(code)),
    date_only     = parse_date(SubmissionDate),
    sex           = trimws(tolower(sex)),
    role          = trimws(tolower(role)),
    region        = trimws(region),
    rural         = trimws(rural)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  3. CLEAN TEACHER QUESTIONNAIRE
#     Key columns: code, SubmissionDate, respondent_code, sex
#     A2 = grade levels taught (multi-select, stored as space-separated string)
#     A3 = subject taught (1=math, 2=science, 3=language, 97=other)
# ══════════════════════════════════════════════════════════════════════════════
teacher_q <- teacher_q %>%
  filter(!is.na(code), trimws(as.character(code)) != "") %>%
  mutate(
    school_code   = trimws(as.character(code)),
    date_only     = parse_date(SubmissionDate),
    sex           = trimws(tolower(sex)),
    region        = trimws(region),
    rural         = trimws(rural)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  4. CLEAN STUDENT QUESTIONNAIRE
#     Key columns: code, SubmissionDate, student_id, assent
#     A1 = sex (1=male, 2=female)
#     A2 = age
#     A3 = grade (1=Grade1 … 5=Grade5+)
#     NOTE: no 'rural' column in student file — will join from school_info
# ══════════════════════════════════════════════════════════════════════════════
student_q <- student_q %>%
  filter(!is.na(code), trimws(as.character(code)) != "") %>%
  mutate(
    school_code   = trimws(as.character(code)),
    date_only     = parse_date(SubmissionDate),
    sex           = case_when(
      as.integer(A1) == 1 ~ "male",
      as.integer(A1) == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    region        = trimws(region)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  5. CLEAN SCHOOL INFORMATION SURVEY
#     Key columns: code, region, zone, woreda, school_name, rural
#     SEC2.*  = enrollment counts (total, by sex, by subject)
#     GPS     = gps_point-Latitude / gps_point-Longitude
#     NOTE: SEC2 values of 999 are likely missing/refused codes — recode to NA
# ══════════════════════════════════════════════════════════════════════════════
school_info <- school_info %>%
  filter(!is.na(code), trimws(as.character(code)) != "") %>%
  # Keep first row per school if duplicated (1 school has 2 rows)
  arrange(code, desc(SubmissionDate)) %>%
  distinct(code, .keep_all = TRUE) %>%
  mutate(
    school_code    = trimws(as.character(code)),
    date_only      = parse_date(SubmissionDate),
    region         = trimws(region),
    rural          = trimws(rural),
    # Recode 999 (missing sentinel) to NA in all SEC2 enrollment columns
    across(starts_with("SEC2"), ~ if_else(. >= 999, NA_real_, as.numeric(.)))
  )

# Slim lookup for joining geography onto other instruments
school_lookup <- school_info %>%
  select(school_code, school_name, region, zone, woreda, rural) %>%
  distinct()

# ══════════════════════════════════════════════════════════════════════════════
#  6. PER-SCHOOL COMPLETION SUMMARY
#     Flags each school as complete/incomplete for each instrument
# ══════════════════════════════════════════════════════════════════════════════

# Count submissions per school per instrument
per_principal <- principal_q %>%
  group_by(school_code) %>%
  summarise(
    n_principal   = n(),
    p_date_last   = max(date_only, na.rm = TRUE),
    p_has_female  = any(sex == "female", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    p_complete = n_principal >= P_TARGET,
    p_missing  = pmax(P_TARGET - n_principal, 0L)
  )

per_teacher <- teacher_q %>%
  group_by(school_code) %>%
  summarise(
    n_teacher     = n(),
    t_date_last   = max(date_only, na.rm = TRUE),
    t_female      = sum(sex == "female", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    t_complete = n_teacher >= T_TARGET,
    t_missing  = pmax(T_TARGET - n_teacher, 0L)
  )

per_student <- student_q %>%
  group_by(school_code) %>%
  summarise(
    n_student     = n(),
    s_date_last   = max(date_only, na.rm = TRUE),
    s_female      = sum(sex == "female", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    s_complete = n_student >= S_TARGET,
    s_missing  = pmax(S_TARGET - n_student, 0L)
  )

per_info <- school_info %>%
  transmute(
    school_code,
    has_info   = TRUE,
    i_date_last = date_only
  )

# Combine into one per-school row
per_school <- school_lookup %>%
  left_join(per_principal, by = "school_code") %>%
  left_join(per_teacher,   by = "school_code") %>%
  left_join(per_student,   by = "school_code") %>%
  left_join(per_info,      by = "school_code") %>%
  replace_na(list(
    n_principal = 0L, p_complete = FALSE, p_missing = P_TARGET,
    n_teacher   = 0L, t_complete = FALSE, t_missing = T_TARGET,
    n_student   = 0L, s_complete = FALSE, s_missing = S_TARGET,
    has_info    = FALSE
  )) %>%
  mutate(
    # Overall completion: all three respondent instruments done
    fully_complete = p_complete & t_complete & s_complete,
    # Date of most recent submission for this school (any instrument)
    date_last = pmax(p_date_last, t_date_last, s_date_last, i_date_last, na.rm = TRUE),
    # Status label
    status = case_when(
      fully_complete        ~ "Complete",
      n_principal > 0 | n_teacher > 0 | n_student > 0 ~ "In Progress",
      TRUE                  ~ "Not Started"
    ),
    # Any instrument still missing
    any_followup = !fully_complete
  )

# ══════════════════════════════════════════════════════════════════════════════
#  7. DAILY SUBMISSION PROGRESS  (all 4 instruments combined)
# ══════════════════════════════════════════════════════════════════════════════
daily_p  <- principal_q %>% filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>% summarise(n_p = n(), .groups = "drop")

daily_t  <- teacher_q %>% filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>% summarise(n_t = n(), .groups = "drop")

daily_s  <- student_q %>% filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>% summarise(n_s = n(), .groups = "drop")

daily_si <- school_info %>% filter(!is.na(date_only)) %>%
  group_by(date = date_only) %>% summarise(n_si = n(), .groups = "drop")

# New schools visited per day (based on first principal submission per school)
first_visit <- principal_q %>%
  group_by(school_code) %>%
  summarise(first_date = min(date_only, na.rm = TRUE), .groups = "drop")

daily_schools <- first_visit %>%
  group_by(date = first_date) %>%
  summarise(new_schools = n(), .groups = "drop")

daily <- full_join(daily_p,  daily_t,  by = "date") %>%
  full_join(daily_s,  by = "date") %>%
  full_join(daily_si, by = "date") %>%
  full_join(daily_schools, by = "date") %>%
  replace_na(list(n_p = 0L, n_t = 0L, n_s = 0L, n_si = 0L, new_schools = 0L)) %>%
  arrange(date) %>%
  mutate(
    n_total        = n_p + n_t + n_s + n_si,
    cum_schools    = cumsum(new_schools),
    cum_p          = cumsum(n_p),
    cum_t          = cumsum(n_t),
    cum_s          = cumsum(n_s),
    date_label     = format(date, "%d-%b"),
    day_num        = as.integer(date - min(date, na.rm = TRUE)) + 1L,
    school_pace    = round(SCHOOL_TARGET / FIELD_DAYS * day_num)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  8. REGIONAL SUMMARY
# ══════════════════════════════════════════════════════════════════════════════
region_sum <- per_school %>%
  group_by(region) %>%
  summarise(
    n_schools    = n(),
    n_complete   = sum(fully_complete),
    n_progress   = sum(status == "In Progress"),
    n_notstarted = sum(status == "Not Started"),
    n_principal  = sum(n_principal),
    n_teacher    = sum(n_teacher),
    n_student    = sum(n_student),
    pct_complete = round(n_complete / n_schools * 100, 1),
    .groups      = "drop"
  ) %>%
  arrange(desc(n_schools))

# ══════════════════════════════════════════════════════════════════════════════
#  9. INSTRUMENT-LEVEL SUMMARY  (for overview value boxes)
# ══════════════════════════════════════════════════════════════════════════════
instrument_sum <- tibble(
  instrument = c("Principal", "Teacher", "Student", "School Info"),
  submitted  = c(nrow(principal_q), nrow(teacher_q), nrow(student_q), nrow(school_info)),
  schools    = c(n_distinct(principal_q$school_code),
                 n_distinct(teacher_q$school_code),
                 n_distinct(student_q$school_code),
                 n_distinct(school_info$school_code)),
  target_per = c(P_TARGET, T_TARGET, S_TARGET, 1L),
  target_tot = c(SCHOOL_TARGET * P_TARGET,
                 SCHOOL_TARGET * T_TARGET,
                 SCHOOL_TARGET * S_TARGET,
                 SCHOOL_TARGET),
  pct        = round(submitted / target_tot * 100, 1)
)

# ══════════════════════════════════════════════════════════════════════════════
#  10. SCHOOL STATUS TABLE  (for Woredas/map tab)
# ══════════════════════════════════════════════════════════════════════════════
school_status <- per_school %>%
  transmute(
    code       = school_code,
    school     = school_name,
    region,
    zone,
    woreda,
    rural,
    status,
    n_p        = as.integer(n_principal),
    n_t        = as.integer(n_teacher),
    n_s        = as.integer(n_student),
    p_ok       = p_complete,
    t_ok       = t_complete,
    s_ok       = s_complete,
    dl         = format(date_last, "%d-%b"),
    pct_done   = round((n_p / P_TARGET + n_t / T_TARGET + n_s / S_TARGET) / 3 * 100, 1)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  11. FOLLOW-UP TABLE  (schools needing action)
# ══════════════════════════════════════════════════════════════════════════════
fu_tbl <- per_school %>%
  filter(any_followup) %>%
  transmute(
    code     = school_code,
    school   = school_name,
    region,
    woreda,
    rural,
    p_sub    = as.integer(n_principal),
    p_miss   = as.integer(p_missing),
    t_sub    = as.integer(n_teacher),
    t_miss   = as.integer(t_missing),
    s_sub    = as.integer(n_student),
    s_miss   = as.integer(s_missing),
    dl       = format(date_last, "%d-%b")
  ) %>%
  arrange(region, woreda)

# ══════════════════════════════════════════════════════════════════════════════
#  12. ENROLLMENT SUMMARY  (from School Information Survey SEC2)
#      SEC2.total, SEC2.girls_total, SEC2.boys_total
#      SEC2.girls_math, SEC2.boys_math, SEC2.girls_eng, SEC2.boys_eng, ...
# ══════════════════════════════════════════════════════════════════════════════
enroll_sum <- school_info %>%
  left_join(school_lookup %>% select(school_code, region, rural),
            by = "school_code") %>%  rename(region = region.x) %>%
  group_by(region) %>%
  summarise(
    n_schools       = n(),
    total_enroll    = sum(`SEC2.total`,        na.rm = TRUE),
    girls_enroll    = sum(`SEC2.girls_total`,  na.rm = TRUE),
    boys_enroll     = sum(`SEC2.boys_total`,   na.rm = TRUE),
    girls_math      = sum(`SEC2.girls_math`,   na.rm = TRUE),
    boys_math       = sum(`SEC2.boys_math`,    na.rm = TRUE),
    girls_eng       = sum(`SEC2.girls_eng`,    na.rm = TRUE),
    boys_eng        = sum(`SEC2.boys_eng`,     na.rm = TRUE),
    girls_lang      = sum(`SEC2.girls_lang`,   na.rm = TRUE),
    boys_lang       = sum(`SEC2.boys_lang`,    na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    pct_girls = round(girls_enroll / (girls_enroll + boys_enroll) * 100, 1)
  ) %>%
  arrange(desc(n_schools))

# ══════════════════════════════════════════════════════════════════════════════
#  13. KPIs  (plain list of scalars for value boxes)
# ══════════════════════════════════════════════════════════════════════════════
k <- list(
  data_date      = format(max(c(
    max(principal_q$date_only, na.rm = TRUE),
    max(teacher_q$date_only,   na.rm = TRUE),
    max(student_q$date_only,   na.rm = TRUE)
  )), "%d-%b-%Y"),

  # Schools
  schools_total   = SCHOOL_TARGET,
  schools_started = sum(per_school$status != "Not Started"),
  schools_done    = sum(per_school$fully_complete),
  schools_pct     = round(sum(per_school$fully_complete) / SCHOOL_TARGET * 100, 1),
  schools_fu      = sum(per_school$any_followup),

  # Instrument submissions
  n_principal     = nrow(principal_q),
  n_teacher       = nrow(teacher_q),
  n_student       = nrow(student_q),
  n_info          = nrow(school_info),

  # Instrument completion
  p_complete      = sum(per_school$p_complete),
  t_complete      = sum(per_school$t_complete),
  s_complete      = sum(per_school$s_complete),

  p_pct           = round(sum(per_school$p_complete) / SCHOOL_TARGET * 100, 1),
  t_pct           = round(sum(per_school$t_complete) / SCHOOL_TARGET * 100, 1),
  s_pct           = round(sum(per_school$s_complete) / SCHOOL_TARGET * 100, 1),

  # Geography
  n_regions       = n_distinct(per_school$region),
  n_rural         = sum(per_school$rural == "Rural",  na.rm = TRUE),
  n_urban         = sum(per_school$rural == "Urban",  na.rm = TRUE),

  # Field period
  date_start      = format(min(principal_q$date_only, na.rm = TRUE), "%d-%b"),
  date_end        = format(max(principal_q$date_only, na.rm = TRUE), "%d-%b"),
  n_field_days    = as.integer(max(principal_q$date_only, na.rm = TRUE) -
                               min(principal_q$date_only, na.rm = TRUE)) + 1L,

  # Teacher sex breakdown
  t_female        = sum(teacher_q$sex == "female", na.rm = TRUE),
  t_male          = sum(teacher_q$sex == "male",   na.rm = TRUE),
  p_female        = sum(principal_q$sex == "female", na.rm = TRUE),
  p_male          = sum(principal_q$sex == "male",   na.rm = TRUE)
)

# ══════════════════════════════════════════════════════════════════════════════
#  14. JSON  –  for browser-side JS table rendering
# ══════════════════════════════════════════════════════════════════════════════
D_js <- toJSON(list(
  daily       = as.data.frame(daily %>%
                  select(date = date_label, n_p, n_t, n_s, n_si,
                         n_total, cum_schools, school_pace)),
  region      = as.data.frame(region_sum),
  instrument  = as.data.frame(instrument_sum),
  school      = as.data.frame(school_status),
  fu          = as.data.frame(fu_tbl),
  enroll      = as.data.frame(enroll_sum)
), auto_unbox = TRUE, na = "null")
