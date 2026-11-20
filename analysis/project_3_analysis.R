# Project 3: Customer Segmentation, Retention Modeling, and Marketing Allocation

suppressPackageStartupMessages({
  library(completejourney)
  library(tidyverse)
  library(factoextra)
})

dir.create("output", showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1. Load and inspect source data
# -----------------------------
data(demographics)
data(transactions_sample)
data(products)

transactions <- transactions_sample %>%
  mutate(transaction_timestamp = as.POSIXct(transaction_timestamp))

feature_start <- as.Date("2017-01-01")
feature_end <- as.Date("2017-09-30")
holdout_end <- as.Date("2017-12-31")

transactions_feature <- transactions %>%
  filter(as.Date(transaction_timestamp) >= feature_start,
         as.Date(transaction_timestamp) <= feature_end)
transactions_holdout <- transactions %>%
  filter(as.Date(transaction_timestamp) > feature_end,
         as.Date(transaction_timestamp) <= holdout_end)

# -----------------------------
# 2. Customer-level feature table
# -----------------------------
feature_tx <- transactions_feature %>%
  left_join(products %>% select(product_id, department, brand, manufacturer_id), by = "product_id")

last_feature_date <- max(as.Date(feature_tx$transaction_timestamp), na.rm = TRUE)

customer_features <- feature_tx %>%
  group_by(household_id) %>%
  summarise(
    recency = as.numeric(last_feature_date - max(as.Date(transaction_timestamp))),
    frequency = n_distinct(basket_id),
    monetary = sum(sales_value, na.rm = TRUE),
    avg_basket = monetary / frequency,
    product_diversity = n_distinct(product_id),
    category_count = n_distinct(department),
    manufacturer_count = n_distinct(manufacturer_id),
    brand_count = n_distinct(brand),
    coupon_transactions = n_distinct(basket_id[coupon_disc > 0]),
    .groups = "drop"
  )

holdout_outcome <- transactions_holdout %>%
  distinct(household_id) %>%
  mutate(retained = 1L)

customer_data <- demographics %>%
  select(household_id, any_of(c("age", "income", "home_ownership", "marital_status", "household_size", "kid_category"))) %>%
  right_join(customer_features, by = "household_id") %>%
  left_join(holdout_outcome, by = "household_id") %>%
  mutate(retained = replace_na(retained, 0L))

write_csv(customer_data, "output/customer_level_data.csv")

# -----------------------------
# 3. PCA and cluster diagnostics
# -----------------------------
behavior_vars <- c(
  "recency", "frequency", "monetary", "avg_basket", "product_diversity",
  "category_count", "manufacturer_count", "brand_count", "coupon_transactions"
)

pca_input <- customer_data %>%
  select(all_of(behavior_vars)) %>%
  mutate(across(everything(), ~ replace_na(.x, median(.x, na.rm = TRUE))))

pca_fit <- prcomp(pca_input, center = TRUE, scale. = TRUE)
pca_variance <- tibble(
  component = paste0("PC", seq_along(pca_fit$sdev)),
  variance = pca_fit$sdev^2,
  proportion = variance / sum(variance),
  cumulative = cumsum(proportion)
)
write_csv(pca_variance, "output/pca_variance.csv")

pca_loadings <- as.data.frame(pca_fit$rotation) %>%
  rownames_to_column("feature")
write_csv(pca_loadings, "output/pca_loadings.csv")

cluster_input <- pca_fit$x[, seq_len(min(5, ncol(pca_fit$x))), drop = FALSE]
cluster_diagnostics <- map_dfr(2:6, function(k) {
  set.seed(1000 + k)
  fit <- kmeans(cluster_input, centers = k, nstart = 50)
  start_fits <- lapply(seq_len(10), function(i) {
    set.seed(10000 + 100 * k + i)
    kmeans(cluster_input, centers = k, nstart = 1)
  })
  silhouette_width <- mean(cluster::silhouette(fit$cluster, dist(cluster_input))[, "sil_width"])
  tibble(
    k = k,
    total_withinss = fit$tot.withinss,
    avg_silhouette = silhouette_width,
    min_cluster_size = min(table(fit$cluster)),
    stability_min_cluster_size = min(vapply(start_fits, function(x) min(table(x$cluster)), numeric(1))),
    stability_max_cluster_size = max(vapply(start_fits, function(x) min(table(x$cluster)), numeric(1)))
  )
})
write_csv(cluster_diagnostics, "output/cluster_diagnostics.csv")

set.seed(167)
chosen_k <- 4
km_fit <- kmeans(cluster_input, centers = chosen_k, nstart = 100)

segmented_data <- customer_data %>%
  mutate(
    cluster = factor(km_fit$cluster),
    PC1 = pca_fit$x[, 1],
    PC2 = pca_fit$x[, 2]
  )

segment_profile <- segmented_data %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    customer_pct = n() / nrow(segmented_data),
    across(all_of(c("recency", "frequency", "monetary", "avg_basket", "product_diversity", "PC1", "PC2")), ~ median(.x, na.rm = TRUE)),
    predicted_return_rate = mean(retained),
    .groups = "drop"
  )
write_csv(segment_profile, "output/segment_profile.csv")

# -----------------------------
# 4. Logistic regression and evaluation
# -----------------------------
model_data <- segmented_data %>%
  select(retained, all_of(behavior_vars), cluster) %>%
  mutate(across(all_of(behavior_vars), ~ replace_na(.x, median(.x, na.rm = TRUE))))

retention_model <- glm(
  retained ~ recency + frequency + monetary + avg_basket + product_diversity + category_count + coupon_transactions + cluster,
  data = model_data,
  family = binomial()
)

model_data <- model_data %>%
  mutate(predicted_probability = predict(retention_model, type = "response"),
         predicted_class = as.integer(predicted_probability >= 0.5))

confusion <- model_data %>%
  count(retained, predicted_class, name = "n")
write_csv(confusion, "output/confusion_matrix.csv")
write_csv(broom::tidy(retention_model, exponentiate = TRUE, conf.int = TRUE), "output/logistic_coefficients.csv")

# A stratified holdout evaluates generalization rather than in-sample fit.
set.seed(2025)
train_idx <- unlist(lapply(split(seq_len(nrow(model_data)), model_data$retained), function(idx) {
  sample(idx, size = floor(0.8 * length(idx)))
}))
train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

fit_no_cluster <- glm(
  retained ~ recency + frequency + monetary + avg_basket + product_diversity + category_count + coupon_transactions,
  data = train_data,
  family = binomial()
)
fit_with_cluster <- glm(
  retained ~ recency + frequency + monetary + avg_basket + product_diversity + category_count + coupon_transactions + cluster,
  data = train_data,
  family = binomial()
)

auc_base <- function(actual, probability) {
  positives <- probability[actual == 1]
  negatives <- probability[actual == 0]
  (sum(outer(positives, negatives, `>`)) + 0.5 * sum(outer(positives, negatives, `==`))) /
    (length(positives) * length(negatives))
}
classification_metrics <- function(actual, probability, cutoff = 0.5) {
  predicted <- as.integer(probability >= cutoff)
  tibble(
    accuracy = mean(predicted == actual),
    sensitivity = sum(predicted == 1 & actual == 1) / sum(actual == 1),
    specificity = sum(predicted == 0 & actual == 0) / sum(actual == 0),
    precision = sum(predicted == 1 & actual == 1) / max(1, sum(predicted == 1)),
    roc_auc = auc_base(actual, probability),
    cutoff = cutoff
  )
}

test_probability <- predict(fit_with_cluster, newdata = test_data, type = "response")
test_metrics <- classification_metrics(test_data$retained, test_probability)
write_csv(test_metrics, "output/test_model_metrics.csv")

comparison <- bind_rows(
  classification_metrics(test_data$retained, predict(fit_no_cluster, newdata = test_data, type = "response")) %>% mutate(model = "Behavior variables only"),
  test_metrics %>% mutate(model = "Behavior variables plus cluster")
) %>% select(model, everything())
write_csv(comparison, "output/model_comparison.csv")

segment_performance <- test_data %>%
  mutate(predicted_probability = test_probability) %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    actual_return_rate = mean(retained),
    mean_predicted_probability = mean(predicted_probability),
    roc_auc = if (n_distinct(retained) == 2) auc_base(retained, predicted_probability) else NA_real_,
    .groups = "drop"
  )
write_csv(segment_performance, "output/segment_model_performance.csv")

calibration <- test_data %>%
  mutate(predicted_probability = test_probability,
         probability_decile = ntile(predicted_probability, 10)) %>%
  group_by(probability_decile) %>%
  summarise(
    customers = n(),
    mean_predicted_probability = mean(predicted_probability),
    observed_return_rate = mean(retained),
    .groups = "drop"
  )
write_csv(calibration, "output/calibration.csv")

model_summary <- tibble(
  accuracy = mean(model_data$retained == model_data$predicted_class),
  sensitivity = sum(model_data$predicted_class == 1 & model_data$retained == 1) / sum(model_data$retained == 1),
  specificity = sum(model_data$predicted_class == 0 & model_data$retained == 0) / sum(model_data$retained == 0),
  note = "These initial metrics are in-sample; replace with a true training/test split before final submission."
)
write_csv(model_summary, "output/model_summary.csv")

# -----------------------------
# 5. Budget allocation
# -----------------------------
# The policy deliberately funds growth and retention differently. These are
# planning assumptions, not causal estimates of incremental lift.
budget_policy <- segment_profile %>%
  mutate(
    segment_label = recode(as.character(cluster),
      `1` = "Frequent broad shoppers",
      `2` = "High-value loyal shoppers",
      `3` = "Growth-potential regulars",
      `4` = "At-risk low-engagement shoppers"),
    marketing_action = recode(as.character(cluster),
      `1` = "Personalized cross-category loyalty offers",
      `2` = "Low-discount retention and VIP access",
      `3` = "Test targeted bundles and frequency incentives",
      `4` = "Low-cost win-back; suppress expensive discounts"),
    budget_pct = recode(as.character(cluster), `1` = 0.25, `2` = 0.20, `3` = 0.40, `4` = 0.15),
    allocation = budget_pct * 1000000,
    justification = recode(as.character(cluster),
      `1` = "Large, valuable base with very high observed return; protect value efficiently.",
      `2` = "Highest value and return; use retention touches rather than broad discounting.",
      `3` = "Largest group with meaningful room to grow; reserve the largest test allocation.",
      `4` = "Lowest value and return; cap cost while testing inexpensive reactivation.")) %>%
  select(segment_label, customers, predicted_return_rate, monetary, marketing_action, allocation, budget_pct, justification)
write_csv(budget_policy, "output/budget_allocation_starting_point.csv")

message("Project 3 analysis complete. Review the rendered report and generated model diagnostics.")
