# =========================================================
# Synthetic data replication script for pricing paradox analyses
# =========================================================

# --------------------- Packages ---------------------
required_pkgs <- c(
  "tidyverse", "janitor", "scales", "lubridate",
  "broom", "patchwork", "glue"
)

to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
library(broom)
library(patchwork)
library(glue)

options(dplyr.summarise.inform = FALSE)

# --------------------- Input ---------------------
# Use your synthetic CSV here
input_file <- "C:/synthetic_police_ir_data.csv"

# --------------------- Helper function ---------------------
norm_cols <- function(df, src) {
  df |>
    mutate(source = src) |>
    mutate(
      event_date = as_date(event_date),
      victim_type = tolower(as.character(victim_type)),
      group = as.character(group),
      
      is_attempt = as.integer(is_attempt),
      paid = as.integer(paid),
      raas = as.integer(raas),
      data_exfiltration = as.integer(data_exfiltration),
      cyber_insurance = as.integer(cyber_insurance),
      ir_firm_involved = as.integer(ir_firm_involved),
      negotiation_attacker = as.integer(negotiation_attacker),
      negotiation_victim = as.integer(negotiation_victim),
      
      negotiation_duration_days = suppressWarnings(as.numeric(negotiation_duration_days)),
      initial_ransom_eur = suppressWarnings(as.numeric(initial_ransom_eur)),
      final_ransom_eur   = suppressWarnings(as.numeric(final_ransom_eur)),
      revenue_eur        = suppressWarnings(as.numeric(revenue_eur)),
      
      backups_status = case_when(
        str_detect(tolower(as.character(backups_status)), "unrecov") ~ "unrecoverable",
        str_detect(tolower(as.character(backups_status)), "partial") ~ "partial",
        str_detect(tolower(as.character(backups_status)), "recov")   ~ "recoverable",
        TRUE ~ "unknown"
      )
    )
}

# --------------------- Read + normalize ---------------------
raw <- read_csv(input_file, show_col_types = FALSE) |>
  clean_names()

# Split by source already present in the synthetic dataset
police_raw <- raw |> filter(source == "police")
ir_raw     <- raw |> filter(source == "ir")

police <- norm_cols(police_raw, "police")
ir     <- norm_cols(ir_raw, "ir")

both <- bind_rows(police, ir)

cat("\n==================== RAW DATA CHECK ====================\n")
cat("Rows in raw file:", nrow(raw), "\n")
cat("Rows police:", nrow(police), "\n")
cat("Rows ir:", nrow(ir), "\n")
cat("Rows combined:", nrow(both), "\n")
print(head(both, 5))

# --------------------- Filter: timeframe & population ---------------------
in_window <- function(d) !is.na(d) & d >= as_date("2019-01-01") & d < as_date("2023-01-01")

filtered <- both |>
  filter(
    in_window(event_date),
    !(is_attempt %in% 1),
    !(tolower(victim_type) %in% c("individual", "person", "private"))
  )

cat("\n==================== FILTERED DATA ====================\n")
cat("Rows after filtering:", nrow(filtered), "\n")

# --------------------- Deduplication ---------------------
# For the synthetic dataset, case_uid is enough
filtered <- filtered |>
  mutate(
    dedup_key = paste(source, case_uid, sep = "|")
  )

combined <- filtered |>
  arrange(desc(source == "police")) |>
  group_by(dedup_key) |>
  slice(1) |>
  ungroup()

cat("\n==================== DEDUPLICATED DATA ====================\n")
cat("Rows after deduplication:", nrow(combined), "\n")
cat("Unique dedup keys:", n_distinct(combined$dedup_key), "\n")

# --------------------- Variable engineering ---------------------
combined <- combined |>
  mutate(
    revenue_log = log10(pmax(replace_na(revenue_eur, 0), 0) + 1),
    
    backups_unrecoverable = as.integer(backups_status == "unrecoverable"),
    backups_partial       = as.integer(backups_status == "partial"),
    backups_recoverable   = as.integer(backups_status == "recoverable"),
    
    initial_ransom_log = log10(pmax(replace_na(initial_ransom_eur, 0), 0) + 1),
    
    negotiation_duration_log = if_else(
      is.na(negotiation_duration_days), NA_real_,
      log10(pmax(negotiation_duration_days, 0) + 1)
    ),
    
    discount_offered = as.integer(
      !is.na(final_ransom_eur) &
        !is.na(initial_ransom_eur) &
        final_ransom_eur < initial_ransom_eur
    ),
    
    discount_size_pct = if_else(
      !is.na(final_ransom_eur) & !is.na(initial_ransom_eur) & initial_ransom_eur > 0,
      pmax(0, (initial_ransom_eur - final_ransom_eur) / initial_ransom_eur) * 100,
      NA_real_
    )
  )


# --------------------- Model helpers ---------------------
has_cols <- function(cols) all(cols %in% names(combined))

logit_or <- function(model) {
  s <- coef(summary(model))
  tibble(
    term = rownames(s),
    estimate = s[, "Estimate"],
    se = s[, "Std. Error"],
    z = s[, "z value"],
    p.value = s[, "Pr(>|z|)"],
    OR = exp(estimate),
    CI.low = exp(estimate - 1.96 * se),
    CI.high = exp(estimate + 1.96 * se)
  ) |>
    select(term, OR, CI.low, CI.high, p.value)
}

# --------------------- M1 ---------------------
cat("\n==================== MODEL M1 ====================\n")
if (has_cols(c("negotiation_attacker","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration"))) {
  m1 <- glm(
    negotiation_attacker ~ revenue_log + raas +
      backups_unrecoverable + backups_partial + backups_recoverable +
      data_exfiltration,
    data = combined,
    family = binomial()
  )
  print(summary(m1))
  print(logit_or(m1), n = Inf, width = Inf)
} else {
  cat("Required columns missing for M1.\n")
}

# --------------------- M2 ---------------------
cat("\n==================== MODEL M2 ====================\n")
if (has_cols(c("negotiation_victim","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "cyber_insurance","ir_firm_involved"))) {
  m2 <- glm(
    negotiation_victim ~ revenue_log + raas +
      backups_unrecoverable + backups_partial + backups_recoverable +
      data_exfiltration + cyber_insurance + ir_firm_involved,
    data = combined,
    family = binomial()
  )
  print(summary(m2))
  print(logit_or(m2), n = Inf, width = Inf)
} else {
  cat("Required columns missing for M2.\n")
}

# --------------------- M3 ---------------------
cat("\n==================== MODEL M3 ====================\n")
if (has_cols(c("discount_offered","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "negotiation_duration_log"))) {
  m3_data <- combined |>
    filter(!is.na(discount_offered), !is.na(negotiation_duration_log))
  
  if (nrow(m3_data) > 5) {
    m3 <- glm(
      discount_offered ~ revenue_log + raas +
        backups_unrecoverable + backups_partial + backups_recoverable +
        data_exfiltration + negotiation_duration_log,
      data = m3_data,
      family = binomial()
    )
    print(summary(m3))
    print(logit_or(m3), n = Inf, width = Inf)
  } else {
    cat("Too few usable rows for M3 after filtering.\n")
  }
} else {
  cat("Required columns missing for M3.\n")
}

# --------------------- M4 ---------------------
cat("\n==================== MODEL M4 ====================\n")
if (has_cols(c("discount_size_pct","initial_ransom_log"))) {
  m4_data <- combined |>
    filter(!is.na(discount_size_pct), !is.na(initial_ransom_log))
  
  if (nrow(m4_data) > 5) {
    m4 <- lm((discount_size_pct / 100) ~ initial_ransom_log, data = m4_data)
    print(summary(m4))
    print(broom::tidy(m4), n = Inf, width = Inf)
  } else {
    cat("Too few usable rows for M4 after filtering.\n")
  }
} else {
  cat("Required columns missing for M4.\n")
}

# --------------------- M5 ---------------------
cat("\n==================== MODEL M5 ====================\n")
if (has_cols(c("paid","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "cyber_insurance","ir_firm_involved","negotiation_victim",
               "negotiation_duration_log","discount_offered","initial_ransom_log"))) {
  m5_data <- combined |>
    filter(
      !is.na(paid),
      !is.na(revenue_log),
      !is.na(raas),
      !is.na(backups_unrecoverable),
      !is.na(backups_partial),
      !is.na(backups_recoverable),
      !is.na(data_exfiltration),
      !is.na(cyber_insurance),
      !is.na(ir_firm_involved),
      !is.na(negotiation_victim),
      !is.na(negotiation_duration_log),
      !is.na(discount_offered),
      !is.na(initial_ransom_log)
    )
  
  if (nrow(m5_data) > 5) {
    m5 <- glm(
      paid ~ revenue_log + raas +
        backups_unrecoverable + backups_partial + backups_recoverable +
        data_exfiltration + cyber_insurance + ir_firm_involved +
        negotiation_victim + negotiation_duration_log +
        discount_offered + initial_ransom_log,
      data = m5_data,
      family = binomial()
    )
    print(summary(m5))
    print(logit_or(m5), n = Inf, width = Inf)
  } else {
    cat("Too few usable rows for M5 after filtering.\n")
  }
} else {
  cat("Required columns missing for M5.\n")
}
