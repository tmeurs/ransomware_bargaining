
path <- "C:/data_neg_flat.csv" #Put here your path for data_neg_flat.csv
data_neg_flat <- read.csv(path) 


################

library(ggplot2)
library(scales)

# -----------------------------
# Helper function
# -----------------------------
parse_amount <- function(x) {
  as.numeric(gsub(",", "", gsub("\\$", "", trimws(as.character(x)))))
}

# -----------------------------
# Create working copy
# -----------------------------
df <- data_neg_flat

# Parse monetary variables to numeric
df$initial_ransom_num <- parse_amount(df$initial_ransom)
df$negotiated_ransom_num <- parse_amount(df$negotiated_ransom)

# Ensure paid is binary/logical
df$paid_bin <- ifelse(df$paid == TRUE, 1, 0)

# Use negotiated ransom if available, otherwise initial ransom, but only for paid cases
df$ransom_paid <- ifelse(
  df$paid == TRUE,
  ifelse(!is.na(df$negotiated_ransom), df$negotiated_ransom, df$initial_ransom),
  NA
)

df$ransom_paid_num <- parse_amount(df$ransom_paid)

# Discount relative to initial ransom
df$discount <- ifelse(
  !is.na(df$initial_ransom_num) &
    !is.na(df$negotiated_ransom_num) &
    df$initial_ransom_num > 0,
  1 - (df$negotiated_ransom_num / df$initial_ransom_num),
  NA
)

# Discount in percentages based on final amount paid
df$discount_pct <- ifelse(
  !is.na(df$initial_ransom_num) &
    !is.na(df$ransom_paid_num) &
    df$initial_ransom_num > 0,
  100 * (df$initial_ransom_num - df$ransom_paid_num) / df$initial_ransom_num,
  NA
)

# -----------------------------
# Basic descriptive checks
# -----------------------------
summary(df$message_count)

boxplot(
  df$message_count,
  main = "Distribution of Message Count",
  ylab = "Number of Messages"
)

prop_paid <- mean(df$paid_bin, na.rm = TRUE)
cat("Proportion paid:", round(prop_paid, 3), "\n")

na_negotiated <- is.na(df$negotiated_ransom_num)
prop_na_negotiated <- mean(na_negotiated, na.rm = TRUE)
cat("Proportion with no negotiated ransom:", round(prop_na_negotiated, 3), "\n")

table(
  NegotiatedRansomMissing = na_negotiated,
  Paid = df$paid_bin
)

boxplot(
  message_count ~ is.na(negotiated_ransom_num),
  data = df,
  names = c("Negotiated Ransom Present", "Missing"),
  main = "Message Count by Negotiated Ransom Availability",
  ylab = "Number of Messages"
)

# -----------------------------
# Create message-count buckets
# -----------------------------
df_bucket <- subset(df, !is.na(message_count))

df_bucket$bucket <- cut(
  df_bucket$message_count,
  breaks = c(-Inf, 10, 25, 50, 100, Inf),
  labels = c("0-10", "11-25", "26-50", "51-100", "100+"),
  right = TRUE
)

bucket_order <- c("0-10", "11-25", "26-50", "51-100", "100+")

all_buckets <- data.frame(
  bucket = factor(bucket_order, levels = bucket_order)
)

# -----------------------------
# Summary table by bucket
# -----------------------------
bucket_counts <- as.data.frame(table(df_bucket$bucket))
colnames(bucket_counts) <- c("bucket", "count")

initial_rate <- aggregate(!is.na(initial_ransom_num) ~ bucket, data = df_bucket, mean)
colnames(initial_rate)[2] <- "initial_pct"
initial_rate$initial_pct <- round(100 * initial_rate$initial_pct, 1)

payment_rate <- aggregate(paid_bin ~ bucket, data = df_bucket, mean)
colnames(payment_rate)[2] <- "paid_pct"
payment_rate$paid_pct <- round(100 * payment_rate$paid_pct, 1)

negotiated_rate <- aggregate(!is.na(negotiated_ransom_num) ~ bucket, data = df_bucket, mean)
colnames(negotiated_rate)[2] <- "negotiated_pct"
negotiated_rate$negotiated_pct <- round(100 * negotiated_rate$negotiated_pct, 1)

median_initial <- aggregate(initial_ransom_num ~ bucket, data = df_bucket, median, na.rm = TRUE)
colnames(median_initial)[2] <- "median_initial"

mean_initial <- aggregate(initial_ransom_num ~ bucket, data = df_bucket, mean, na.rm = TRUE)
colnames(mean_initial)[2] <- "mean_initial"

median_negotiated <- aggregate(negotiated_ransom_num ~ bucket, data = df_bucket, median, na.rm = TRUE)
colnames(median_negotiated)[2] <- "median_negotiated"

mean_negotiated <- aggregate(negotiated_ransom_num ~ bucket, data = df_bucket, mean, na.rm = TRUE)
colnames(mean_negotiated)[2] <- "mean_negotiated"

paid_data <- subset(df_bucket, paid_bin == 1 & !is.na(ransom_paid_num))

median_paid <- aggregate(ransom_paid_num ~ bucket, data = paid_data, median, na.rm = TRUE)
colnames(median_paid)[2] <- "median_paid"

mean_paid <- aggregate(ransom_paid_num ~ bucket, data = paid_data, mean, na.rm = TRUE)
colnames(mean_paid)[2] <- "mean_paid"

discount_stats <- aggregate(discount_pct ~ bucket, data = df_bucket, function(x) {
  c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE))
})

discount_stats_df <- data.frame(
  bucket = discount_stats$bucket,
  mean_discount_pct = round(discount_stats$discount_pct[, "mean"], 1),
  median_discount_pct = round(discount_stats$discount_pct[, "median"], 1)
)

summary_table <- Reduce(
  function(x, y) merge(x, y, by = "bucket", all = TRUE),
  list(
    all_buckets,
    bucket_counts,
    initial_rate,
    payment_rate,
    negotiated_rate,
    median_initial,
    mean_initial,
    median_negotiated,
    mean_negotiated,
    median_paid,
    mean_paid,
    discount_stats_df
  )
)

summary_table <- summary_table[match(bucket_order, summary_table$bucket), ]

# Round monetary variables
money_vars <- c(
  "median_initial", "mean_initial",
  "median_negotiated", "mean_negotiated",
  "median_paid", "mean_paid"
)

summary_table[money_vars] <- lapply(summary_table[money_vars], function(x) round(x, 0))

print(summary_table, row.names = FALSE)

# -----------------------------
# Plot 1: Discount by bucket
# -----------------------------
ggplot(summary_table, aes(x = bucket)) +
  geom_line(aes(y = mean_discount_pct, group = 1, color = "Mean discount"), size = 1) +
  geom_point(aes(y = mean_discount_pct, color = "Mean discount"), size = 3) +
  geom_line(aes(y = median_discount_pct, group = 1, color = "Median discount"), size = 1) +
  geom_point(aes(y = median_discount_pct, color = "Median discount"), size = 3) +
  scale_color_manual(values = c("Mean discount" = "blue", "Median discount" = "orange")) +
  labs(
    x = "Number of Messages Exchanged During Negotiation",
    y = "Discount Given (%)",
    title = "Discount Given by Negotiation Length",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------
# Plot 2: Message count and discount
# -----------------------------
df$discount[is.na(df$discount)]<-0
plot_df <- subset(df, !is.na(message_count) & !is.na(discount))


ggplot(plot_df, aes(x = message_count, y = discount)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Negotiation Message Count and Ransom Discount",
    x = "Number of Negotiation Messages",
    y = "Discount Given (Proportion)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 150)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# -----------------------------
# Summary table by ransomware group
# -----------------------------
df$group <- as.factor(df$group)
groups <- unique(df$group)

summary_by_group <- do.call(rbind, lapply(groups, function(g) {
  subset_g <- subset(df, group == g)
  
  if (nrow(subset_g) == 0) return(NULL)
  
  data.frame(
    group = g,
    count = nrow(subset_g),
    initial_pct = round(mean(!is.na(subset_g$initial_ransom_num) & subset_g$initial_ransom_num > 0) * 100, 1),
    paid_pct = round(mean(!is.na(subset_g$ransom_paid_num) & subset_g$ransom_paid_num > 0) * 100, 1),
    negotiated_pct = round(mean(!is.na(subset_g$negotiated_ransom_num) & subset_g$negotiated_ransom_num > 0) * 100, 1),
    median_initial = ifelse(all(is.na(subset_g$initial_ransom_num) | subset_g$initial_ransom_num == 0), NA,
                            median(subset_g$initial_ransom_num[subset_g$initial_ransom_num > 0], na.rm = TRUE)),
    mean_initial = ifelse(all(is.na(subset_g$initial_ransom_num) | subset_g$initial_ransom_num == 0), NA,
                          mean(subset_g$initial_ransom_num[subset_g$initial_ransom_num > 0], na.rm = TRUE)),
    median_negotiated = ifelse(all(is.na(subset_g$negotiated_ransom_num) | subset_g$negotiated_ransom_num == 0), NA,
                               median(subset_g$negotiated_ransom_num[subset_g$negotiated_ransom_num > 0], na.rm = TRUE)),
    mean_negotiated = ifelse(all(is.na(subset_g$negotiated_ransom_num) | subset_g$negotiated_ransom_num == 0), NA,
                             mean(subset_g$negotiated_ransom_num[subset_g$negotiated_ransom_num > 0], na.rm = TRUE)),
    median_paid = ifelse(all(is.na(subset_g$ransom_paid_num) | subset_g$ransom_paid_num == 0), NA,
                         median(subset_g$ransom_paid_num[subset_g$ransom_paid_num > 0], na.rm = TRUE)),
    mean_paid = ifelse(all(is.na(subset_g$ransom_paid_num) | subset_g$ransom_paid_num == 0), NA,
                       mean(subset_g$ransom_paid_num[subset_g$ransom_paid_num > 0], na.rm = TRUE)),
    mean_discount_pct = ifelse(all(is.na(subset_g$discount)), NA,
                               round(mean(subset_g$discount, na.rm = TRUE) * 100, 1)),
    median_discount_pct = ifelse(all(is.na(subset_g$discount)), NA,
                                 round(median(subset_g$discount, na.rm = TRUE) * 100, 1)),
    mean_messages = round(mean(subset_g$message_count, na.rm = TRUE), 1),
    median_messages = median(subset_g$message_count, na.rm = TRUE)
  )
}))

print(summary_by_group, row.names = FALSE)

# -----------------------------
# Logistic regression models
# -----------------------------
logit_df <- subset(
  df,
  !is.na(paid_bin) &
    !is.na(message_count) &
    !is.na(discount) &
    !is.na(group) &
    !is.na(initial_ransom_num)
)

model_paid <- glm(
  paid_bin ~ log10(message_count + 1) + discount + log10(initial_ransom_num + 1),
  data = logit_df,
  family = binomial
)

summary(model_paid)

df$discount_given <- ifelse(df$discount > 0, 1, 0)

disc_df <- subset(
  df,
  !is.na(discount) &
    !is.na(message_count) &
    !is.na(group) &
    !is.na(initial_ransom_num)
)

model_discount <- glm(
  discount_given ~ log10(message_count + 1) + log10(initial_ransom_num + 1),
  data = df,
  family = binomial
)

summary(model_discount)

# Odds ratios for payment model
or_paid <- exp(coef(model_paid))
ci_paid <- exp(confint(model_paid))

or_table_paid <- data.frame(
  OR = round(or_paid, 3),
  CI_lower = round(ci_paid[, 1], 3),
  CI_upper = round(ci_paid[, 2], 3)
)

print(or_table_paid, row.names = FALSE)

# Odds ratios for discount model
or_discount <- exp(coef(model_discount))
ci_discount <- exp(confint(model_discount))

or_table_discount <- data.frame(
  OR = round(or_discount, 3),
  CI_lower = round(ci_discount[, 1], 3),
  CI_upper = round(ci_discount[, 2], 3)
)

print(or_table_discount, row.names = FALSE)