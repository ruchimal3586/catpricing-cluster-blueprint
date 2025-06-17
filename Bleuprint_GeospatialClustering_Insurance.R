
setwd("C:/Users/ruchi/OneDrive/Documents/S&P/Insurance/catpricing_cluster_blueprint")
rm(list=ls())

# License and Reuse Notice
# ------------------------------
# This project is licensed under the MIT License. You may use, modify, and share this code with attribution.
# See the LICENSE file for full terms.

# Citation and Credit
# ------------------------------
# If you use or adapt this blueprint, please cite the GitHub repository:
# Malhotra, Ruchi. (2024). Cat Pricing Cluster Blueprint. GitHub repository. https://github.com/ruchimal3586/catpricing-cluster-blueprint


  
  # Cat Model Simulation + Exposure Clustering with Real Hazard Data (CHIRPS)
  
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(cluster)
library(tidyr)
library(purrr)
library(scales)
library(data.table)

set.seed(42)

# ------------------------------
# 1. Generate Random Exposure Points in California
# ------------------------------

n_assets <- 500
exposure <- tibble(
  asset_id = 1:n_assets,
  lat = runif(n_assets, 32.5, 42),     # Approx. lat range for CA
  lon = runif(n_assets, -124.5, -114), # Approx. lon range for CA
  insured_value = runif(n_assets, 200000, 1000000),
  construction = sample(c("wood", "masonry", "steel"), n_assets, replace = TRUE),
  occupancy = sample(c("residential", "commercial"), n_assets, replace = TRUE),
  deductible = 5000,
  limit = 750000
)

# Convert to spatial object
asset_pts <- terra::vect(exposure, geom = c('lon', 'lat'), crs = 4326)

# ------------------------------
# 2. Load CHIRPS Hazard Rasters
# ------------------------------
hazard_2014 <- rast("chirps-v3.0.2014.tif")
hazard_2024 <- rast("chirps-v3.0.2024.tif")

# Exposure raste value assignments
exposure$precip_2014 <- terra::extract(hazard_2014, asset_pts)[[2]]
exposure$precip_2024 <- terra::extract(hazard_2024, asset_pts)[[2]]

# ------------------------------
# 3. Compute Precip Change & Fire Risk Proxy
# ------------------------------
exposure <- exposure %>%
  mutate(
    precip_change = precip_2024 - precip_2014,
    fire_risk_score = log1p(pmax(0, precip_change))
    )  # high precip change is higher risk proxy


# ------------------------------
# 4. Feature Engineering for Clustering
# ------------------------------
df_cluster <- exposure %>%
  mutate(
    construction_num = as.numeric(factor(construction)),
    occupancy_num = as.numeric(factor(occupancy))
  ) %>%
  select(insured_value, precip_2024, fire_risk_score, construction_num, occupancy_num) %>%
  drop_na()

# ------------------------------
# 4b. Elbow Method
# ------------------------------
wcss <- purrr::map_dbl(1:10, function(k) {
  kmeans(df_cluster, centers = k, nstart = 10)$tot.withinss
})

elbow_df <- tibble(k = 1:10, wcss = wcss)

ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_line(color = "darkblue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Elbow Method: Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares")

# ------------------------------
# 5. Perform K-Means Clustering
# ------------------------------
k <- 5
km_model <- kmeans(df_cluster, centers = k)
exposure$cluster <- as.factor(km_model$cluster)

#6. Simulate CAT Load with Stochastic Event Losses and Financial Logic
# ------------------------------
vuln_lookup <- c(wood = 0.6, masonry = 0.4, steel = 0.2)
exposure$vuln_factor <- vuln_lookup[exposure$construction]

# Simulate 1000 stochastic events
n_events <- 1000
event_catalog <- tibble(
  event_id = 1:n_events,
  severity = rexp(n_events, rate = 1/1.5)  # heavy-tail shock distribution
)

# Cross-join exposure to events,  Use tidyr::crossing for full join while preserving exposure columns
#sim_matrix <- crossing(event_catalog, exposure) %>%
 # mutate(
  ##  raw_loss = insured_value * vuln_factor * severity * fire_risk_score * 0.0001,
  ##  gross_loss = pmax(0, raw_loss - deductible),
  ##  net_loss = pmin(gross_loss, limit))

# Repeat exposure 1000 times and bind event_catalog with matching rows

sim_matrix <- expand.grid(asset_id = exposure$asset_id, event_id = event_catalog$event_id) %>%
  left_join(exposure, by = "asset_id") %>%
  left_join(event_catalog, by = "event_id") 


sim_matrix <- sim_matrix %>%
  mutate(
    raw_loss = insured_value * vuln_factor * severity * fire_risk_score * 0.01,
    gross_loss = pmax(0, raw_loss - deductible),
    net_loss = pmin(gross_loss, limit)
  )


# Compute average loss per asset (AAL)
aal_df <- sim_matrix %>%
  group_by(asset_id) %>%
  summarise(cat_load = mean(net_loss, na.rm = TRUE))

exposure <- exposure %>% left_join(aal_df, by = "asset_id")

# Simulate expected premium via GLM
base_rate <- 0.02
construction_factors <- c(wood = 1.2, masonry = 1.0, steel = 0.9)
exposure <- exposure %>%
  mutate(expected_premium = insured_value * base_rate * construction_factors[construction])

# ------------------------------
# 7. Cluster-Level Summary
# ------------------------------
cluster_summary <- exposure %>%
  group_by(cluster) %>%
  summarise(
    avg_insured_value = mean(insured_value),
    avg_fire_risk = mean(fire_risk_score),
    avg_cat_load = mean(cat_load, na.rm = TRUE),
    avg_expected_premium = mean(expected_premium),
    total_cat_load = sum(cat_load, na.rm = TRUE),
    count = n(),
    avg_loss_ratio = mean(cat_load / expected_premium, na.rm = TRUE)
  )

print(cluster_summary)

library(knitr)
library(kableExtra)
cluster_table <- cluster_summary %>%
  kable(digits = 2, format = "html", caption = "Cluster-Level Summary of CAT Load and Premium Metrics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)



# ------------------------------
# 8. Visualize CAT Load vs Expected Premium by Cluster
# ------------------------------
ggplot(cluster_summary, aes(x = cluster)) +
  geom_col(aes(y = avg_cat_load, fill = "CAT Load"), position = "dodge") +
  geom_col(aes(y = avg_expected_premium, fill = "Expected Premium"), position = "dodge") +
  scale_fill_manual(name = "Metric", values = c("CAT Load" = "firebrick", "Expected Premium" = "steelblue")) +
  labs(title = "Average CAT Load vs Expected Premium by Cluster",
       x = "Cluster",
       y = "Amount ($)") +
  theme_minimal()

# Additional Visualization: Total CAT Load by Cluster

ggplot(cluster_summary, aes(x = cluster, y = total_cat_load, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total CAT Load by Cluster",
       x = "Cluster",
       y = "Total CAT Load ($)") +
  theme_minimal()

# Additional Visualization: CAT Load to Premium Ratio

ggplot(cluster_summary, aes(x = cluster, y = avg_loss_ratio, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Loss Ratio (CAT Load / Premium) by Cluster",
       x = "Cluster",
       y = "Loss Ratio") +
  theme_minimal()


# 9. EP Curves by Cluster
# ------------------------------
sim_matrix_dt <- as.data.table(sim_matrix)

ep_curves <- sim_matrix_dt[, .(quantile = seq(0, 0.99, 0.01)), by = .(cluster)]
ep_curves <- merge(
  ep_curves,
  sim_matrix_dt[, .(net_loss = quantile(net_loss, probs = seq(0, 0.99, 0.01), na.rm = TRUE)), by = .(cluster)],
  by = c("cluster", "quantile")
)

ggplot(ep_curves, aes(x = quantile, y = net_loss, color = cluster)) +
  geom_line(size = 1.2) +
  labs(title = "EP Curve by Cluster", x = "Exceedance Probability (1 - p)", y = "Loss ($)") +
  theme_minimal()



# ------------------------------
# 9. Export CSV
# ------------------------------
write.csv(exposure, "exposure_with_clusters_and_catload.csv", row.names = FALSE)
write.csv(cluster_summary, "cluster_level_summary.csv", row.names = FALSE)


