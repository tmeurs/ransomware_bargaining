# police_data_pricing_paradox.R
# Reproduces "Police Data" analyses: Pricing Paradox tables and figure.
# Input: "police_data.csv" and "ir_data.csv"
# Outputs (attach with submission):
#   artifacts/negotiation_stages.csv
#   artifacts/discount_by_ransom.csv
#   artifacts/negotiationoutcomes2.jpg
#   artifacts/combined_clean.csv
#   (optional) artifacts/M*_OR.csv for five-stage models

# --------------------- Setup ---------------------
required_pkgs <- c(
  "tidyverse", "janitor", "scales", "lubridate",
  "broom", "patchwork", "glue", "digest"
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
library(digest)

set.seed(42)
dir.create("artifacts", showWarnings = FALSE)

# --------------------- Inputs ---------------------
# Expected columns (rename below if your headers differ):
# - case_uid (unique id if available)         - source ("police"/"ir")
# - event_date (YYYY-MM-DD)                   - victim_type ("company"/"individual")
# - is_attempt (0/1)                          - initial_ransom_eur (numeric)
# - final_ransom_eur (numeric or NA)          - paid (0/1)
# - group (ransomware group string)           - raas (0/1)
# - backups_status (factor: unrecoverable/partial/recoverable/unknown)
# - data_exfiltration (0/1)                   - revenue_eur (numeric)
# - cyber_insurance (0/1)                     - ir_firm_involved (0/1)
# - negotiation_attacker (0/1 or NA)          - negotiation_victim (0/1 or NA)
# - negotiation_duration_days (numeric or NA)

police_file <- "police_data.csv"
ir_file     <- "ir_data.csv"

police_raw <- read_csv(police_file, show_col_types = FALSE) |> clean_names()
ir_raw     <- read_csv(ir_file,     show_col_types = FALSE) |> clean_names()

# --------------------- Harmonize + Bind ---------------------
norm_cols <- function(df, src) {
  df |>
    mutate(source = src) |>
    rename_with(~ gsub("amount", "ransom", .x)) |>
    rename(
      case_uid = coalesce(case_uid, NA_character_),
      event_date = coalesce(event_date, date, incident_date),
      victim_type = coalesce(victim_type, victim_category),
      is_attempt = coalesce(is_attempt, attempt),
      initial_ransom_eur = coalesce(initial_ransom_eur, initial_ransom),
      final_ransom_eur   = coalesce(final_ransom_eur, final_ransom),
      paid = coalesce(paid, payment_made),
      group = coalesce(group, ransomware_group),
      raas = coalesce(raas, is_raas),
      backups_status = coalesce(backups_status, backup_status),
      data_exfiltration = coalesce(data_exfiltration, exfiltration),
      revenue_eur = coalesce(revenue_eur, revenue),
      cyber_insurance = coalesce(cyber_insurance, insurance),
      ir_firm_involved = coalesce(ir_firm_involved, ir_involved),
      negotiation_attacker = coalesce(negotiation_attacker, attacker_negotiated),
      negotiation_victim   = coalesce(negotiation_victim, victim_negotiated),
      negotiation_duration_days = coalesce(negotiation_duration_days, negotiation_days)
    ) |>
    mutate(
      event_date = suppressWarnings(as_date(event_date)),
      victim_type = tolower(as.character(victim_type)),
      is_attempt = as.integer(is_attempt %in% c(1, TRUE, "1", "true", "yes")),
      paid = as.integer(paid %in% c(1, TRUE, "1", "true", "yes")),
      raas = as.integer(raas %in% c(1, TRUE, "1", "true", "yes")),
      data_exfiltration = as.integer(data_exfiltration %in% c(1, TRUE, "1", "true", "yes")),
      cyber_insurance = as.integer(cyber_insurance %in% c(1, TRUE, "1", "true", "yes")),
      ir_firm_involved = as.integer(ir_firm_involved %in% c(1, TRUE, "1", "true", "yes")),
      negotiation_attacker = as.integer(negotiation_attacker %in% c(1, TRUE, "1", "true", "yes")),
      negotiation_victim   = as.integer(negotiation_victim   %in% c(1, TRUE, "1", "true", "yes")),
      negotiation_duration_days = suppressWarnings(as.numeric(negotiation_duration_days)),
      initial_ransom_eur = suppressWarnings(as.numeric(initial_ransom_eur)),
      final_ransom_eur   = suppressWarnings(as.numeric(final_ransom_eur)),
      revenue_eur        = suppressWarnings(as.numeric(revenue_eur)),
      backups_status = case_when(
        str_detect(tolower(as.character(backups_status)), "unrecov") ~ "unrecoverable",
        str_detect(tolower(as.character(backups_status)), "partial")  ~ "partial",
        str_detect(tolower(as.character(backups_status)), "recov")    ~ "recoverable",
        TRUE ~ "unknown"
      )
    )
}

police <- norm_cols(police_raw, "police")
ir     <- norm_cols(ir_raw,     "ir")

both <- bind_rows(police, ir)

# --------------------- Filter: timeframe & population ---------------------
in_window <- function(d) !is.na(d) & d >= as_date("2019-01-01") & d < as_date("2023-01-01")
filtered <- both |>
  filter(
    in_window(event_date),
    !(is_attempt %in% 1),
    !(tolower(victim_type) %in% c("individual", "person", "private"))
  )

# --------------------- Deduplication ---------------------
# Prefer existing case_uid. Otherwise, build a hash from date + group + initial ransom (+ coarse sector if present).
make_fallback_key <- function(df) {
  paste(
    df$event_date,
    tolower(replace_na(df$group, "")),
    round(replace_na(df$initial_ransom_eur, -1)),
    tolower(replace_na(df$victim_sector, "")),
    sep = "|"
  )
}

filtered <- filtered |>
  mutate(
    dedup_key = if_else(
      !is.na(case_uid) & case_uid != "",
      paste(source, case_uid, sep = "|"),
      paste(source, digest(make_fallback_key(cur_data_all()), algo = "xxhash64"), sep = "|")
    )
  )

# Keep one record per incident prioritizing police over IR (for shared cases)
combined <- filtered |>
  arrange(desc(source == "police")) |>
  group_by(dedup_key) |>
  slice(1) |>
  ungroup()

# --------------------- Variable engineering (1a–7a) ---------------------
combined <- combined |>
  mutate(
    # 1a
    revenue_log = log10(pmax(replace_na(revenue_eur, 0), 0) + 1),
    # 2a/2b/2c (mutually exclusive dummies)
    backups_unrecoverable = as.integer(backups_status == "unrecoverable"),
    backups_partial       = as.integer(backups_status == "partial"),
    backups_recoverable   = as.integer(backups_status == "recoverable"),
    # 3b
    initial_ransom_log = log10(pmax(replace_na(initial_ransom_eur, 0), 0) + 1),
    # 5c
    negotiation_duration_log = if_else(
      is.na(negotiation_duration_days), NA_real_,
      log10(pmax(negotiation_duration_days, 0) + 1)
    ),
    # 6a/6b
    discount_offered = as.integer(!is.na(final_ransom_eur) &
                                    !is.na(initial_ransom_eur) &
                                    final_ransom_eur < initial_ransom_eur),
    discount_size_pct = if_else(
      !is.na(final_ransom_eur) & !is.na(initial_ransom_eur) & initial_ransom_eur > 0,
      pmax(0, (initial_ransom_eur - final_ransom_eur) / initial_ransom_eur) * 100,
      NA_real_
    )
  )

write_csv(combined, file.path("artifacts", "combined_clean.csv"))

# --------------------- Helper: Ransom bins ---------------------
rbin_breaks <- c(-Inf, 1e3, 1e4, 1e5, 1e6, 1e7, Inf)
rbin_labels <- c("<1,000", "<10,000", "<100,000", "<1 million", "<10 million", ">10 million")

combined <- combined |>
  mutate(
    ransom_bin = cut(initial_ransom_eur, breaks = rbin_breaks, labels = rbin_labels, right = FALSE),
    # Strategy categories for Table "Negotiation Outcomes by Initial Ransom and Strategy"
    strategy = case_when(
      (is.na(negotiation_victim) | negotiation_victim == 0) ~ "No negotiation",
      negotiation_victim == 1 & (is.na(discount_offered) | discount_offered == 0) ~ "Negotiation no discount",
      negotiation_victim == 1 & discount_offered == 1 ~ "Negotiation discount",
      TRUE ~ "No negotiation"
    ),
    outcome = if_else(paid == 1, "Pay", "No Pay")
  )

# --------------------- Table 1: negotiation_stages.csv ---------------------
# Wide cross-tab: rows=ransom_bin, columns=sub-cells for each strategy x outcome
t1_counts <- combined |>
  count(ransom_bin, strategy, outcome, name = "n") |>
  complete(ransom_bin, strategy, outcome, fill = list(n = 0)) |>
  mutate(strategy = factor(strategy,
                           levels = c("No negotiation", "Negotiation no discount", "Negotiation discount")),
         outcome  = factor(outcome, levels = c("No Pay", "Pay"))) |>
  arrange(ransom_bin, strategy, outcome)

t1_wide <- t1_counts |>
  unite(col = "col", strategy, outcome, sep = " - ") |>
  pivot_wider(names_from = col, values_from = n) |>
  arrange(factor(ransom_bin, levels = rbin_labels))

# For convenience, ensure all expected columns exist (even if zeros)
expected_cols <- c(
  "No negotiation - No Pay", "No negotiation - Pay",
  "Negotiation no discount - No Pay", "Negotiation no discount - Pay",
  "Negotiation discount - No Pay", "Negotiation discount - Pay",
  "Final ransom - No Pay", "Final ransom - Pay" # placeholder for readability; we’ll fill below
)
for (cc in expected_cols) if (!cc %in% names(t1_wide)) t1_wide[[cc]] <- 0L

# "Final ransom" columns are counts of outcomes among discount cases (same as above, but used for layout parity)
final_cols <- combined |>
  filter(strategy == "Negotiation discount") |>
  count(ransom_bin, outcome, name = "n") |>
  complete(ransom_bin, outcome, fill = list(n = 0)) |>
  mutate(outcome = factor(outcome, levels = c("No Pay", "Pay")))

t1_wide <- t1_wide |>
  left_join(final_cols |> filter(outcome == "No Pay") |> select(ransom_bin, n) |> rename(`Final ransom - No Pay` = n),
            by = "ransom_bin") |>
  left_join(final_cols |> filter(outcome == "Pay")    |> select(ransom_bin, n) |> rename(`Final ransom - Pay` = n),
            by = "ransom_bin") |>
  mutate(across(c(`Final ransom - No Pay`, `Final ransom - Pay`), ~replace_na(.x, 0L))) |>
  relocate(ransom_bin,
           `No negotiation - No Pay`, `No negotiation - Pay`,
           `Negotiation no discount - No Pay`, `Negotiation no discount - Pay`,
           `Negotiation discount - No Pay`, `Negotiation discount - Pay`,
           `Final ransom - No Pay`, `Final ransom - Pay`) |>
  rename(`Range (b1) [euro]` = ransom_bin)

write_csv(t1_wide, file.path("artifacts", "negotiation_stages.csv"))
message("Wrote: artifacts/negotiation_stages.csv")

# --------------------- Table 2: discount_by_ransom.csv ---------------------
# Discount bins: 0–25, 26–50, 51–75, 76–100 (percent)
dbins <- c(-Inf, 25, 50, 75, 100, Inf)
dlabels <- c("0--25%", "26--50%", "51--75%", "76--100%", ">100%")

t2_counts <- combined |>
  filter(strategy == "Negotiation discount", !is.na(discount_size_pct)) |>
  mutate(
    discount_bucket = cut(discount_size_pct, breaks = dbins, labels = dlabels, right = TRUE),
    outcome = factor(outcome, levels = c("No Pay", "Pay"))
  ) |>
  count(ransom_bin, discount_bucket, outcome, name = "n") |>
  complete(ransom_bin, discount_bucket, outcome, fill = list(n = 0)) |>
  arrange(factor(ransom_bin, levels = rbin_labels), discount_bucket, outcome)

t2_wide <- t2_counts |>
  unite(col = "col", discount_bucket, outcome, sep = " - ") |>
  pivot_wider(names_from = col, values_from = n) |>
  arrange(factor(ransom_bin, levels = rbin_labels)) |>
  rename(`Initial Ransom [euro]` = ransom_bin)

write_csv(t2_wide, file.path("artifacts", "discount_by_ransom.csv"))
message("Wrote: artifacts/discount_by_ransom.csv")

# --------------------- Figure: negotiationoutcomes2.jpg ---------------------
# Panel A: Payment rate by initial ransom bin and strategy
panel_a <- combined |>
  mutate(strategy = factor(strategy,
                           levels = c("No negotiation", "Negotiation no discount", "Negotiation discount"))) |>
  group_by(ransom_bin, strategy) |>
  summarize(
    n = n(),
    pay_rate = mean(paid == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = ransom_bin, y = pay_rate, fill = strategy)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = scales::percent(pay_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  scale_y_continuous("Payment rate", labels = percent_format(), limits = c(0, 1)) +
  scale_x_discrete("Initial ransom (euro)") +
  labs(title = "Negotiation outcomes by ransom tier and strategy") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Panel B (for discount cases): initial vs final ransom (median & IQR) by bin
disc_stats <- combined |>
  filter(strategy == "Negotiation discount") |>
  group_by(ransom_bin) |>
  summarize(
    med_initial = median(initial_ransom_eur, na.rm = TRUE),
    q1_initial  = quantile(initial_ransom_eur, 0.25, na.rm = TRUE),
    q3_initial  = quantile(initial_ransom_eur, 0.75, na.rm = TRUE),
    med_final   = median(final_ransom_eur, na.rm = TRUE),
    q1_final    = quantile(final_ransom_eur, 0.25, na.rm = TRUE),
    q3_final    = quantile(final_ransom_eur, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = -ransom_bin, names_to = "metric", values_to = "value") |>
  separate(metric, into = c("stat", "which"), sep = "_") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(which = factor(which, levels = c("initial", "final"), labels = c("Initial demand", "Final demand")))

panel_b <- disc_stats |>
  ggplot(aes(x = ransom_bin, y = med, group = which, linetype = which)) +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_line(position = position_dodge(width = 0.3)) +
  scale_y_continuous("Ransom (EUR, median with IQR)", labels = label_number_si(accuracy = 0.1)) +
  scale_x_discrete("Initial ransom (bin)") +
  labs(title = "Discount cases: initial vs final ransom by bin", linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Combine panels and save
fig <- panel_a / panel_b + plot_layout(heights = c(3, 2))
ggsave(file.path("artifacts", "negotiationoutcomes2.jpg"), fig, width = 10, height = 8, units = "in", dpi = 300)
message("Wrote: artifacts/negotiationoutcomes2.jpg")

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

# M1: Attacker Negotiation (if available) ~ predictors
if (has_cols(c("negotiation_attacker","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration"))) {
  m1 <- glm(negotiation_attacker ~ revenue_log + raas +
              backups_unrecoverable + backups_partial + backups_recoverable +
              data_exfiltration,
            data = combined, family = binomial())
  write_csv(logit_or(m1), file.path("artifacts", "M1_OR.csv"))
}

# M2: Victim Negotiation ~ predictors (+ insurance, IR)
if (has_cols(c("negotiation_victim","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "cyber_insurance","ir_firm_involved"))) {
  m2 <- glm(negotiation_victim ~ revenue_log + raas +
              backups_unrecoverable + backups_partial + backups_recoverable +
              data_exfiltration + cyber_insurance + ir_firm_involved,
            data = combined, family = binomial())
  write_csv(logit_or(m2), file.path("artifacts", "M2_OR.csv"))
}

# M3: Discount Given ~ predictors (+ negotiation duration)
if (has_cols(c("discount_offered","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "negotiation_duration_log"))) {
  m3 <- glm(discount_offered ~ revenue_log + raas +
              backups_unrecoverable + backups_partial + backups_recoverable +
              data_exfiltration + negotiation_duration_log,
            data = combined, family = binomial())
  write_csv(logit_or(m3), file.path("artifacts", "M3_OR.csv"))
}

# M4: Discount Size (fraction) ~ initial_ransom_log (+ covariates)  — linear
if (has_cols(c("discount_size_pct","initial_ransom_log"))) {
  m4 <- lm((discount_size_pct/100) ~ initial_ransom_log, data = combined |> filter(!is.na(discount_size_pct)))
  broom::tidy(m4) |>
    write_csv(file.path("artifacts", "M4_coef.csv"))
}

# M5: Payment ~ predictors (+ negotiation victim, discount, duration, initial ransom)
if (has_cols(c("paid","revenue_log","raas","backups_unrecoverable",
               "backups_partial","backups_recoverable","data_exfiltration",
               "cyber_insurance","ir_firm_involved","negotiation_victim",
               "negotiation_duration_log","discount_offered","initial_ransom_log"))) {
  m5 <- glm(paid ~ revenue_log + raas +
              backups_unrecoverable + backups_partial + backups_recoverable +
              data_exfiltration + cyber_insurance + ir_firm_involved +
              negotiation_victim + negotiation_duration_log +
              discount_offered + initial_ransom_log,
            data = combined, family = binomial())
  write_csv(logit_or(m5), file.path("artifacts", "M5_OR.csv"))
}

cat("\nDone.\n")
