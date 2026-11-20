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
  fit <- kmeans(cluster_input, centers = k, nstart = 50)
  tibble(k = k, total_withinss = fit$tot.withinss, min_cluster_size = min(table(fit$cluster)))
})
write_csv(cluster_diagnostics, "output/cluster_diagnostics.csv")

set.seed(167)
chosen_k <- 4
km_fit <- kmeans(cluster_input, centers = chosen_k, nstart = 100)

segmented_data <- customer_data %>%
  mutate(cluster = factor(km_fit$cluster))

segment_profile <- segmented_data %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    customer_pct = n() / nrow(segmented_data),
    across(all_of(c("recency", "frequency", "monetary", "avg_basket", "product_diversity")), ~ median(.x, na.rm = TRUE)),
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

model_summary <- tibble(
  accuracy = mean(model_data$retained == model_data$predicted_class),
  sensitivity = sum(model_data$predicted_class == 1 & model_data$retained == 1) / sum(model_data$retained == 1),
  specificity = sum(model_data$predicted_class == 0 & model_data$retained == 0) / sum(model_data$retained == 0),
  note = "These initial metrics are in-sample; replace with a true training/test split before final submission."
)
write_csv(model_summary, "output/model_summary.csv")

# -----------------------------
# 5. Initial budget allocation
# -----------------------------
# This is a starting policy, not a causal estimate. Final allocations should
# be revised after reviewing segment value, risk, and campaign-cost assumptions.
budget_policy <- segment_profile %>%
  mutate(
    segment_label = paste("Cluster", cluster),
    marketing_action = "Review after profiling",
    allocation = 1000000 * customer_pct,
    budget_pct = allocation / 1000000
  ) %>%
  select(segment_label, customers, predicted_return_rate, monetary, marketing_action, allocation, budget_pct)
write_csv(budget_policy, "output/budget_allocation_starting_point.csv")

message("Project 3 starter analysis complete. Review files in output/ before finalizing labels and budget logic.")
