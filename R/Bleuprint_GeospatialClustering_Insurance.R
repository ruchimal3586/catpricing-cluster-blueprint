

# License and Reuse Notice
# ------------------------------
# This project is licensed under the MIT License. You may use, modify, and share this code with attribution.
# See the LICENSE file for full terms.

# Citation and Credit
# ------------------------------
# If you use or adapt this blueprint, please cite the GitHub repository:
# Malhotra, Ruchi. (2024). Cat Pricing Cluster Blueprint. GitHub repository. https://github.com/ruchimal3586/catpricing-cluster-blueprint


# Compact mini-cat pipeline: clustering + stochastic losses + EP/PML/TVaR
# Reads rasters from data/, writes CSVs to outputs/. No setwd().

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(scales)
  library(ggplot2); library(terra); library(sf); library(cluster)
  library(data.table); library(kableExtra); library(here)
})

set.seed(42)

# Paths (relative to repo root)
hazard_2014_path <- here::here("data", "chirps-v3.0.2014.tif")
hazard_2024_path <- here::here("data", "chirps-v3.0.2024.tif")
out_dir          <- here::here("outputs")
fig_dir          <- file.path(out_dir, "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(hazard_2014_path), file.exists(hazard_2024_path))


## 2. Generate Random Exposure Points in California

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
asset_pts <- terra::vect(exposure, geom = c("lon","lat"), crs = "EPSG:4326")
print(crs(asset_pts))

# ------------------------------
# 2. Load CHIRPS Hazard Rasters
# ------------------------------
hazard_2014_path <- here::here("data", "chirps-v3.0.2014.tif")
hazard_2024_path <- here::here("data", "chirps-v3.0.2024.tif")

stopifnot(file.exists(hazard_2014_path), file.exists(hazard_2024_path))

hazard_2014 <- terra::rast(hazard_2014_path)
hazard_2024 <- terra::rast(hazard_2024_path)

# Exposure raster value assignments
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

#6. Simulate CAT Load with Stochastic Event Losses and Policy terms
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


# Compute average loss per asset (AAL) per asset
aal_df <- sim_matrix %>%
  group_by(asset_id) %>%
  summarise(cat_load = mean(net_loss, na.rm = TRUE))

exposure <- exposure %>% left_join(aal_df, by = "asset_id")

# Simulate expected premium via GLM using a simple proxy
base_rate <- 0.02
construction_factors <- c(wood = 1.2, masonry = 1.0, steel = 0.9)
exposure <- exposure %>%
  mutate(expected_premium = insured_value * base_rate * construction_factors[construction])


# 6b. GLM Fit with Regularization (LASSO)
x_vars <- model.matrix(cat_load ~ insured_value + fire_risk_score + factor(construction) + factor(occupancy), data = exposure)[, -1]
y_var <- exposure$cat_load

lasso_model <- cv.glmnet(x_vars, y_var, alpha = 1)  # Gaussian is default
print(lasso_model)

predicted <- predict(lasso_model, newx = x_vars, s = "lambda.min")
residuals <- y_var - predicted

hist(residuals, breaks = 40, main = "LASSO GLM Residuals", xlab = "Residuals")

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
    avg_loss_ratio = mean(cat_load / expected_premium, na.rm = TRUE),
    .groups ="drop"
  )


# ------------------------------
# 8. EP curves + PML/TVaR (cluster level)
# ------------------------------
dt <- as.data.table(sim_matrix)
#Total loss an event causes to a cluster
event_cluster_losses <- dt[, .(event_loss = sum(net_loss, na.rm=TRUE)),
                           by = .(cluster, event_id)]

# helper, given vector x: all event losses for a cluster, PML captures quantile at alpha=0.99 or return period 100 years
#and tvar captures tail severity or average of all losses higher than quantile
pml_tvar <- function(x, alpha = 0.99){
  q <- as.numeric(quantile(x, probs = alpha, na.rm = TRUE))  # e.g., 0.99 => PML(100)
  tail_vals <- x[x >= q]
  tvar <- if (length(tail_vals)) mean(tail_vals) else q
  list(pml = q, tvar = tvar)
}

#For each cluster, compute: PML_100 = 99th percentile, TVaR_99 = mean of the worst 1% losses,
#PML_200 = 99.5th percentile,TVaR_99_5= mean of the worst 0.5% losses.

metrics <- event_cluster_losses[, {
  m99  <- pml_tvar(event_loss, 0.99)    # ~1-in-100
  m995 <- pml_tvar(event_loss, 0.995)   # ~1-in-200
  .(PML_100 = m99$pml, TVaR_99 = m99$tvar,
    PML_200 = m995$pml, TVaR_99_5 = m995$tvar)
}, by = cluster]



# ------------------------------
# 9. Save CSVs
# ------------------------------
write.csv(exposure,        file.path(out_dir, "exposure_with_clusters_and_catload.csv"), row.names = FALSE)
write.csv(cluster_summary, file.path(out_dir, "cluster_level_summary.csv"),              row.names = FALSE)
write.csv(metrics,         file.path(out_dir, "cluster_pml_tvar_summary.csv"),          row.names = FALSE)


# ------------------------------
# 10. Visualize CAT Load vs Expected Premium by Cluster
# ------------------------------
p1 <- ggplot(cluster_summary, aes(x = cluster)) +
  geom_col(aes(y = avg_cat_load, fill = "CAT Load"), position = "dodge") +
  geom_col(aes(y = avg_expected_premium, fill = "Expected Premium"), position = "dodge") +
  scale_fill_manual(name = "Metric", values = c("CAT Load" = "firebrick", "Expected Premium" = "steelblue")) +
  labs(title = "Average CAT Load vs Expected Premium by Cluster",
       x = "Cluster",
       y = "Amount ($)") +
  theme_minimal()

ggsave(file.path(fig_dir, "catload_vs_premium.png"), p1, width = 8, height = 5, dpi = 150)




# 11. Build EP data for one cluster and annotate PMLs
# ------------------------------


# Build EP data for one cluster and annotate PMLs
one_cluster <- levels(exposure$cluster)[1]
probs <- seq(0.001, 0.99, by = 0.001)
ep <- event_cluster_losses[, .(
  loss = quantile(event_loss, probs = 1 - probs, na.rm = TRUE),
  ep   = probs
), by = cluster]

mrow <- metrics %>% filter(cluster == one_cluster)
p2 <- ep %>% filter(cluster == one_cluster) %>%
  ggplot(aes(ep, loss)) +
  geom_line() +
  geom_hline(yintercept = mrow$PML_100, linetype = "dashed") +
  geom_hline(yintercept = mrow$PML_200, linetype = "dotted") +
  scale_x_reverse(labels = scales::percent) +
  labs(title = paste("EP with PML Markers — Cluster", one_cluster),
       x = "Exceedance Probability", y = "Loss ($)") +
  theme_minimal()

ggsave(file.path(fig_dir, "ep_with_pml_one_cluster.png"), p2, width = 8, height = 5, dpi = 150)

message("Done. CSVs in outputs/, figures in outputs/figures/")


