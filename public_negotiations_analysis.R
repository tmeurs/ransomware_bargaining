# public_negotiations_analysis.R
# Reproduces "Public Ransomware Negotiations" analyses and figures.
# Input file: "public_negotiations.csv"
# Saves: bucket_summary.csv, discount_model_or.csv, payment_model_or.csv,
#        discount_msg.pdf, mes_count_discount.pdf

# --------------------- Setup ---------------------
required_pkgs <- c("tidyverse", "broom", "scales")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(tidyverse)
library(broom)
library(scales)

set.seed(42)
dir.create("artifacts", showWarnings = FALSE)

# --------------------- Data ----------------------
# Expected columns in CSV (one row per negotiation/chat):
# - group: factor/string (ransomware group)
# - message_count: integer (number of exchanged messages)
# - initial_ransom_usd: numeric (initial demand in USD)
# - negotiated_amount_usd: numeric or NA (final amount if any)
# - discount_given: 0/1 (1 if a reduction from initial was offered/accepted)
# - paid: 0/1 (victim paid)


# Path to your dataset (change as needed):
infile <- "public_negotiations.csv"

dat <- read_csv(infile, show_col_types = FALSE) |>
  janitor::clean_names() |>
  rename(
    group = group,
    message_count = message_count,
    initial_ransom = initial_ransom_usd,
    negotiated_amount = negotiated_amount_usd,
    discount = discount_given,
    paid = paid
  ) |>
  mutate(
    # standardize types and safe transforms
    group = as.factor(group),
    message_count = as.integer(message_count),
    initial_ransom = as.numeric(initial_ransom),
    negotiated_amount = suppressWarnings(as.numeric(negotiated_amount)),
    discount = as.integer(discount),
    paid = as.integer(paid),
    any_negotiation = if_else(!is.na(negotiated_amount) & negotiated_amount > 0, 1L, 0L),
    discount_pct = if_else(
      !is.na(negotiated_amount) & initial_ransom > 0,
      pmax(0, (initial_ransom - negotiated_amount) / initial_ransom) * 100,
      NA_real_
    ),
    log10_msgs = log10(pmax(0, message_count) + 1),
    log10_init = log10(pmax(0, initial_ransom) + 1)
  )

stopifnot(nrow(dat) > 0)

# --------------------- Bucket table ----------------------
cut_breaks <- c(-Inf, 10, 25, 50, 100, Inf)
cut_labels <- c("0--10", "11--25", "26--50", "51--100", "100+")

bucket_tbl <- dat |>
  mutate(
    bucket = cut(message_count, breaks = cut_breaks, labels = cut_labels, right = TRUE)
  ) |>
  group_by(bucket) |>
  summarize(
    n = n(),
    init_pct = mean(!is.na(initial_ransom) & initial_ransom > 0) * 100,
    paid_pct = mean(paid == 1, na.rm = TRUE) * 100,
    negot_pct = mean(any_negotiation == 1, na.rm = TRUE) * 100,
    med_init = median(initial_ransom, na.rm = TRUE),
    mean_init = mean(initial_ransom, na.rm = TRUE),
    med_negot = suppressWarnings(median(negotiated_amount[any_negotiation == 1], na.rm = TRUE)),
    mean_negot = suppressWarnings(mean(negotiated_amount[any_negotiation == 1], na.rm = TRUE)),
    mean_disc_pct = mean(discount_pct, na.rm = TRUE),
    med_disc_pct = median(discount_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Format dollar columns in millions as in the table (e.g., 0.53M)
    Med.Init = ifelse(is.finite(med_init), paste0(number(med_init/1e6, accuracy = 0.01), "M"), "--"),
    Mean.Init = ifelse(is.finite(mean_init), paste0(number(mean_init/1e6, accuracy = 0.01), "M"), "--"),
    Med.Neg = ifelse(is.finite(med_negot), paste0(number(med_negot/1e6, accuracy = 0.01), "M"), "0"),
    Mean.Neg = ifelse(is.finite(mean_negot), paste0(number(mean_negot/1e6, accuracy = 0.01), "M"), "0"),
    Init.pct = round(init_pct, 1),
    Paid.pct = round(paid_pct, 1),
    Negot.pct = round(negot_pct, 1),
    Mean.Disc.pct = round(mean_disc_pct, 1),
    Median.Disc.pct = round(med_disc_pct, 1)
  ) |>
  select(
    Bucket = bucket, n,
    `Init. %` = Init.pct,
    `Paid %` = Paid.pct,
    `Negot. %` = Negot.pct,
    `Med. Init` = Med.Init,
    `Mean Init` = Mean.Init,
    `Med. Neg` = Med.Neg,
    `Mean Neg` = Mean.Neg,
    `Mean Disc. %` = Mean.Disc.pct,
    `Median Disc. %` = Median.Disc.pct
  )

write_csv(bucket_tbl, file.path("artifacts", "bucket_summary.csv"))
message("Wrote: artifacts/bucket_summary.csv")

# --------------------- Models ----------------------
# Model 1: discount ~ log10(message_count+1) + log10(initial_ransom+1)
m1 <- glm(discount ~ log10_msgs + log10_init,
          data = dat |> filter(!is.na(discount), !is.na(log10_msgs), !is.na(log10_init)),
          family = binomial())

# Model 2: paid ~ log10(message_count+1) * discount + log10(initial_ransom+1)
m2 <- glm(paid ~ log10_msgs * discount + log10_init,
          data = dat |> filter(!is.na(paid), !is.na(discount), !is.na(log10_msgs), !is.na(log10_init)),
          family = binomial())

# Function to convert to Odds Ratios with 95% CI
or_table <- function(model) {
  est <- coef(summary(model))
  tibble(
    term = rownames(est),
    estimate = est[, "Estimate"],
    std_error = est[, "Std. Error"],
    z = est[, "z value"],
    p.value = est[, "Pr(>|z|)"]
  ) |>
    mutate(
      OR = exp(estimate),
      CI.low = exp(estimate - 1.96 * std_error),
      CI.high = exp(estimate + 1.96 * std_error)
    ) |>
    select(term, OR, CI.low, CI.high, p.value)
}

m1_or <- or_table(m1)
m2_or <- or_table(m2)

write_csv(m1_or, file.path("artifacts", "discount_model_or.csv"))
write_csv(m2_or, file.path("artifacts", "payment_model_or.csv"))
message("Wrote: artifacts/discount_model_or.csv, artifacts/payment_model_or.csv")

# --------------------- Figures ----------------------
# Figure 1 (discount_msg.pdf): relationship between message volume and likelihood of discount
# We show a logistic smoother over log10(message_count+1).
p1 <- dat |>
  ggplot(aes(x = log10_msgs, y = discount)) +
  geom_jitter(height = 0.05, width = 0.02, alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = binomial()), se = TRUE) +
  scale_x_continuous(
    name = "log10(Message count + 1)",
    breaks = 0:3,
    labels = c("0", "1", "2", "3")
  ) +
  scale_y_continuous(name = "Predicted probability of discount", labels = percent_format()) +
  labs(title = "Relationship between message volume and likelihood of discount") +
  theme_minimal(base_size = 12)

ggsave(filename = file.path("artifacts", "discount_msg.pdf"), plot = p1, width = 6, height = 4, units = "in")
message("Wrote: artifacts/discount_msg.pdf")

# Figure 2 (mes_count_discount.pdf): probability of payment vs. message volume,
# with/without discount (interaction visualization).
p2 <- dat |>
  mutate(discount = factor(discount, levels = c(0,1), labels = c("No discount", "Discount"))) |>
  ggplot(aes(x = log10_msgs, y = paid, color = discount)) +
  geom_jitter(height = 0.05, width = 0.02, alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = binomial()), se = TRUE) +
  scale_x_continuous(
    name = "log10(Message count + 1)",
    breaks = 0:3,
    labels = c("0", "1", "2", "3")
  ) +
  scale_y_continuous(name = "Predicted probability of payment", labels = percent_format()) +
  labs(title = "Payment likelihood by message volume and discount") +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(title = "Discount"))

ggsave(filename = file.path("artifacts", "mes_count_discount.pdf"), plot = p2, width = 6, height = 4, units = "in")
message("Wrote: artifacts/mes_count_discount.pdf")

# --------------------- Console summaries (optional) ----------------------
cat("\n=== Model 1: Discount given ~ log10(msg+1) + log10(initial+1) ===\n")
print(m1_or |> mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n=== Model 2: Paid ~ log10(msg+1) * Discount + log10(initial+1) ===\n")
print(m2_or |> mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\nDone. \n")
