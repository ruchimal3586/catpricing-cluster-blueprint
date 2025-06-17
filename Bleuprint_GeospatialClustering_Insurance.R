
setwd("C:/Users/ruchi/OneDrive/Documents/S&P/Insurance/Riskexplorer")
rm(list=ls())

# Libraries
install.packages(c("shiny", "leaflet", "tidyverse", "shinyWidgets", "dplyr", "data.table", "readr"))

library(tidyverse)
library(sf)        # for spatial data
library(cluster)   # clustering
library(ggplot2)
library(factoextra)  # for visualization
library(dbscan)
library(shiny)

# Step 1: Load Data (mockup example)
data <- read_csv("your_geospatial_insurance_data.csv")  # includes lat, lon, loss_history, etc.

# Step 2: Convert to spatial
sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# Step 3: Feature prep (select risk-relevant features)
features <- data %>%
  select(loss_history, building_value, elevation, flood_score, wind_score)

# Step 4: Standardize
features_scaled <- scale(features)

# Step 5: Apply k-means
set.seed(123)
km_res <- kmeans(features_scaled, centers = 5)

# Add cluster to original data
data$cluster <- as.factor(km_res$cluster)

# Step 6: Visualize
ggplot(data, aes(x = lon, y = lat, color = cluster)) +
  geom_point() + theme_minimal() +
  labs(title = "Risk-Based Geospatial Clustering")

# Next: Use 'cluster' as a feature in your GLM or ML pricing model


SECOND ONE

# Cat Model Simulation + Exposure Clustering with Real Hazard Data (CHIRPS)

library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(cluster)
library(tidyr)

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
  occupancy = sample(c("residential", "commercial"), n_assets, replace = TRUE)
)

# Convert to spatial object
#exposure_sf <- st_as_sf(exposure, coords = c("lon", "lat"), crs = 4326)
asset_pts <- terra::vect(exposure, geom = c('lon', 'lat'), crs = 4326)


# ------------------------------
# 2. Load CHIRPS Hazard Rasters
# ------------------------------
hazard_2014 <- rast("chirps-v3.0.2014.tif")
hazard_2024 <- rast("chirps-v3.0.2024.tif")

# Extract hazard values at exposure points
#exposure$hazard_2014 <- terra::extract(hazard_2014, vect(exposure_sf))[,2]
exposure$hazard_2014 <- terra::extract(hazard_2014, asset_pts) 
exposure$hazard_2024 <- terra::extract(hazard_2024, asset_pts) 


# ------------------------------
# 3. Compute Change in Hazard
# ------------------------------
exposure <- exposure %>%
  mutate(hazard_change = hazard_2024 - hazard_2014)

# ------------------------------
# 4. Feature Engineering for Clustering
# ------------------------------
# Create numeric encoding for construction and occupancy
df_cluster <- exposure %>%
  mutate(
    construction_num = as.numeric(factor(construction)),
    occupancy_num = as.numeric(factor(occupancy))
  ) %>%
  select(insured_value, hazard_2024, hazard_change, construction_num, occupancy_num) %>%
  drop_na()

# ------------------------------
# 4b. Determine Optimal Clusters with Elbow Method
# ------------------------------

library(purrr)


wcss <- map_dbl(1:10, function(k) {
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
set.seed(42)
k <- 5  # Number of clusters
km_model <- kmeans(df_cluster, centers = k)

exposure$cluster <- as.factor(km_model$cluster)

#How the Elbow Method Works
#Run kmeans() for a range of k (e.g., from 1 to 10).Record the total within-cluster sum of squares (WCSS) for each k.

#Plot k vs. WCSS. Look for the "elbow" point — where adding more clusters doesn’t significantly reduce WCSS.


# ------------------------------
# 6. Visualize the Clusters
# ------------------------------
exposure_sf <- st_as_sf(exposure, coords = c("lon", "lat"), crs = 4326)



ggplot() +
  geom_sf(data = exposure_sf, aes(color = cluster), size = 2, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Exposure Clusters Based on Hazard and Asset Features",
       color = "Cluster")

# ------------------------------
# 7. Export to CSV
# ------------------------------
write.csv(exposure, "exposure_with_clusters.csv", row.names = FALSE)

cluster_summary <- exposure %>%
  group_by(cluster) %>%
  summarise(
    avg_insured_value = mean(insured_value),
    avg_hazard_2024 = mean(hazard_2024$`chirps-v3.0.2024`),
    avg_hazard_change = mean(hazard_change$`chirps-v3.0.2024`),
    count = n()
  )

print(cluster_summary)

library(knitr)
library(kableExtra)
cluster_table <- cluster_summary %>%
  kable(digits = 2, format = "html", caption = "Residual Summary by Construction Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

--------------
  
  
  # Cat Model Simulation + Exposure Clustering with Real Hazard Data (CHIRPS)
  
  library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(cluster)
library(tidyr)
library(purrr)
library(scales)

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
  occupancy = sample(c("residential", "commercial"), n_assets, replace = TRUE)
)

# Convert to spatial object
asset_pts <- terra::vect(exposure, geom = c('lon', 'lat'), crs = 4326)

# ------------------------------
# 2. Load CHIRPS Hazard Rasters
# ------------------------------
hazard_2014 <- rast("chirps-v3.0.2014.tif")
hazard_2024 <- rast("chirps-v3.0.2024.tif")

# Extract hazard values at exposure points
exposure$hazard_2014 <- terra::extract(hazard_2014, asset_pts) 
exposure$hazard_2024 <- terra::extract(hazard_2024, asset_pts) 

# ------------------------------
# 3. Compute Precip Change & Fire Risk Proxy
# ------------------------------
exposure <- exposure %>%
  mutate(
    precip_change = precip_2024 - precip_2014,
    fire_risk_score = -1 * precip_change  # less rain → more fire risk
  )

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

6. Simulate CAT Load with Stochastic Event Losses and Financial Logic
# ------------------------------
vuln_lookup <- c(wood = 0.6, masonry = 0.4, steel = 0.2)
exposure$vuln_factor <- vuln_lookup[exposure$construction]

# Simulate 1000 stochastic events
n_events <- 1000
event_catalog <- tibble(
  event_id = 1:n_events,
  severity = rexp(n_events, rate = 1/1.5)  # heavy-tail shock distribution
)

# Cross-join exposure to events
sim_matrix <- merge(exposure, event_catalog) %>%
  mutate(
    raw_loss = insured_value * vuln_factor * severity * fire_risk_score * 0.0001,
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
    count = n()
  )

print(cluster_summary)

library(knitr)
library(kableExtra)
cluster_table <- cluster_summary %>%
  kable(digits = 2, format = "html", caption = "Residual Summary by Construction Type") %>%
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

# ------------------------------
# 9. Export CSV
# ------------------------------
write.csv(exposure, "exposure_with_clusters_and_catload.csv", row.names = FALSE)
write.csv(cluster_summary, "cluster_level_summary.csv", row.names = FALSE)




##NOT RELEVANT 1 -- IT WAS PART OF ABOVE BUT ENHANCED ABOVE
# ------------------------------
# 6. Simulate CAT Load and Expected Premium via GLM
# ------------------------------
# Define vulnerability factor by construction
vuln_lookup <- c(wood = 0.6, masonry = 0.4, steel = 0.2)
exposure$vuln_factor <- vuln_lookup[exposure$construction]

# CAT Load = fire_risk_score × vulnerability × insured value × base_cat_rate
base_cat_rate <- 0.00005
exposure <- exposure %>%
  mutate(cat_load = pmax(0, fire_risk_score) * vuln_factor * insured_value * base_cat_rate)

# Simulate expected premium via a GLM-like structure
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
    avg_cat_load = mean(cat_load),
    avg_expected_premium = mean(expected_premium),
    total_cat_load = sum(cat_load),
    count = n()
  )

print(cluster_summary)
##NOT RELEVANT 1 -- ENDS
