library(sf)
library(dplyr)
library(httr)
library(readr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(sfhotspot)
library(lubridate)
library(leaflet)
library(leaflet.extras)       # FIX: ADDED for addHeatmap() function
library(htmlwidgets)
library(viridis)
library(fable)                # ML: For time series modeling
library(tsibble)              # ML: For time series data structure
library(feasts)               # ML: For time series feature extraction
library(forecast)             # Already loaded, but good for context
library(dbscan)               # ML: For Density-Based Spatial Clustering

# --- 1. SETUP & BOUNDARIES ---

# Prepare council boundary for Kingston and Sutton boroughs
# (Using a temp file to avoid cluttering working directory)
london_zip <- tempfile(fileext = ".zip")
download.file("https://data.london.gov.uk/download/38460723-837c-44ec-b9f0-1ebe939de89a/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile = london_zip, mode = "wb")

exdir <- tempdir()
unzip(london_zip, exdir = exdir)
shp_path <- file.path(exdir, "statistical-gis-boundaries-london", "ESRI")

# UPDATED: Widened area for comparative analysis
target_boroughs <- c("Kingston upon Thames", "Sutton", "Merton", "Richmond upon Thames")

# Load Boroughs
boroughs_sf <- st_read(file.path(shp_path, "London_Borough_Excluding_MHW.shp"), quiet = TRUE) |>
  filter(NAME %in% target_boroughs) |>
  st_transform(27700)

target_boundary <- st_union(boroughs_sf)
target_boundary_sf <- st_sf(geometry = target_boundary)

# Load Wards (API)
ward_geojson_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_May_2024_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
wards_sf <- st_read(ward_geojson_url, quiet = TRUE) |>
  st_transform(27700)

# Filter wards spatially
wards_centroids <- st_centroid(wards_sf)
target_wards_sf <- wards_sf[st_intersects(wards_centroids, target_boundary_sf, sparse = FALSE)[,1], ]

# --- 2. DATA EXTRACTION ---

download_dir <- tempdir()
latest_url <- "https://data.police.uk/data/archive/latest.zip"
latest_zip <- file.path(download_dir, "latest.zip")

# Only download if not already cached (saves time on re-runs)
if (!file.exists(latest_zip)) {
  message("Downloading Crime Data (this may take a moment)...")
  resp <- httr::GET(latest_url, httr::write_disk(latest_zip, overwrite = TRUE), httr::progress())
}

utils::unzip(latest_zip, exdir = download_dir)

# Helper to read data
extract_data <- function(month, base_dir, boundary_sf) {
  month_dir <- file.path(base_dir, month)
  mps_file <- list.files(month_dir, pattern = paste0("^", month, "-metropolitan-street\\.csv$"), full.names = TRUE)
  
  if (length(mps_file) == 0) return(NULL)
  
  # Read CSV (Optimized reading)
  df <- read_csv(mps_file[1], col_types = cols_only(
    `Crime ID` = col_character(),
    Month = col_character(),
    Longitude = col_double(),
    Latitude = col_double(),
    `Crime type` = col_character(),
    Location = col_character()
  ), progress = FALSE) %>%
    filter(!is.na(Longitude), !is.na(Latitude))
  
  if (nrow(df) == 0) return(NULL)
  
  # Spatial Filter
  st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(27700) %>%
    st_filter(boundary_sf)
}

# Get last 12 months (Reduced from 36 for portfolio demo speed, easy to increase)
folders <- list.dirs(path = download_dir, full.names = FALSE, recursive = FALSE)
month_folders <- grep("^\\d{4}-\\d{2}$", folders, value = TRUE)
target_months <- sort(month_folders, decreasing = TRUE)[1:12] 

message("Processing months: ", paste(target_months, collapse = ", "))

crime_sf <- map(target_months, ~extract_data(.x, download_dir, target_boundary_sf)) %>%
  bind_rows()

# --- 3. THE "PHD VALUE ADD": CRIME HARM INDEX (CHI) ---

# Weights based on the National Crime Harm Index (NCHI) methodology
harm_weights <- c(
  "Violence and sexual offences" = 450, 
  "Robbery" = 1000,
  "Possession of weapons" = 350, 
  "Burglary" = 400, 
  "Criminal damage and arson" = 50, 
  "Drugs" = 300, 
  "Vehicle crime" = 100, 
  "Theft from the person" = 30, 
  "Shoplifting" = 20, 
  "Anti-social behaviour" = 0, 
  "Other theft" = 10,
  "Other crime" = 1
)

crime_scored <- crime_sf %>%
  mutate(
    # Recode crime types to weights, default to 1
    severity = recode(`Crime type`, !!!harm_weights, .default = 1),
    date = ymd(paste0(Month, "-01"))
  )

# --- 4. ANALYSIS: VOLUME VS. HARM ---

# Summarize by Type
summary_table <- crime_scored %>%
  st_drop_geometry() %>%
  group_by(`Crime type`) %>%
  summarise(
    Incidents = n(),
    Total_Harm_Score = sum(severity)
  ) %>%
  arrange(desc(Total_Harm_Score)) # Sort by HARM, not volume

# VISUALIZATION: CRIME HARM RANKING (New PNG)
p_ranking <- summary_table %>%
  head(10) %>% # Focus on the top 10 most harmful
  mutate(`Crime type` = reorder(`Crime type`, Total_Harm_Score)) %>%
  ggplot(aes(x = `Crime type`, y = Total_Harm_Score, fill = Total_Harm_Score)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "cividis") +
  coord_flip() + # Horizontal bars for readability
  labs(
    title = "Top 10 Crime Types by Total Harm Score (NCHI)",
    subtitle = "Focusing resources on crime types that generate the highest societal cost.",
    x = NULL,
    y = "Total Harm Score (Custodial Days)",
    fill = "Harm Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p_ranking)
ggsave("harm_score_ranking.png", p_ranking, width = 10, height = 7)


# --- 5. VISUALIZATION 1: GGPLOT TIME SERIES (Harm Weighted) ---

crime_time_agg <- crime_scored %>%
  st_drop_geometry() %>%
  group_by(date, `Crime type`) %>%
  summarise(daily_harm = sum(severity), .groups = 'drop') %>%
  # Filter for top 5 most harmful categories for cleaner plot
  filter(`Crime type` %in% head(summary_table$`Crime type`, 5))

p1 <- ggplot(crime_time_agg, aes(x = date, y = daily_harm, color = `Crime type`)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Crime Trends by Harm Severity (Not Just Counts)",
    subtitle = "Aggregated using Modified National Crime Harm Index (NCHI) Weights",
    y = "Total Harm Score (Severity * Volume)",
    x = "Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
ggsave("harm_trends.png", p1, width = 10, height = 6)

# --- 6. ML APPLICATION 1: TIME SERIES FORECASTING OF TOTAL HARM ---

# 1. Aggregate to monthly total harm and convert to tsibble
total_harm_ts <- crime_scored %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarise(total_harm = sum(severity)) %>%
  ungroup() %>%
  # The tsibble structure is necessary for modern forecasting models
  as_tsibble(index = date)

# 2. Fit an Exponential Triple Smoothing (ETS) model 
# ETS automatically models Trend and Seasonality, a robust choice.
fit_ets <- total_harm_ts %>%
  model(ETS(total_harm))

# 3. Forecast 12 months ahead
forecast_harm <- fit_ets %>%
  forecast(h = "12 months")

# 4. Plot the forecast with 80% confidence interval
p2 <- autoplot(total_harm_ts, total_harm) +
  autolayer(forecast_harm, total_harm, level = 80, fill = "lightblue") +
  labs(
    title = "12-Month Forecast of Total Crime Harm Score (NCHI)",
    subtitle = "ETS Model Forecast with 80% Prediction Interval",
    y = "Total Harm Score (Custodial Days)",
    x = "Month"
  ) +
  theme_minimal(base_size = 14) +
  guides(colour = "none", fill = "none")

print(p2)
ggsave("harm_forecast.png", p2, width = 10, height = 6)


# --- 7. DBSCAN CLUSTERING: IDENTIFYING MICRO-HOTSPOTS ---

# We must project back to WGS84 (Lat/Lon) for Leaflet
crime_wgs84 <- crime_scored %>%
  st_transform(4326)

# Filter for "Ultra High Harm" events (Severity >= 300 days)
high_harm_events <- crime_wgs84 %>% 
  filter(severity >= 300) # Violence, Robbery, Drugs, Weapons, Burglary

# 1. Prepare data for DBSCAN (needs Cartesian coordinates for distance)
# Convert to a projected coordinate system (27700 - British National Grid) for meters
high_harm_projected <- high_harm_events %>% 
  st_transform(27700)

# Extract coordinates as a matrix
coords <- st_coordinates(high_harm_projected)

# 2. Run DBSCAN
# eps = 75 meters (radius for micro-hotspot), minPts = 10 events
set.seed(42) # For reproducibility
db <- dbscan(coords, eps = 75, minPts = 10)

# 3. Add cluster ID back to the data
high_harm_events$cluster_id <- db$cluster

# 4. Summarize clusters (ignore noise/outliers: cluster_id == 0)
crime_clusters_stats <- high_harm_events %>%
  filter(cluster_id > 0) %>%
  group_by(cluster_id) %>%
  summarise(
    Total_Cluster_Harm = sum(severity),
    Incidents = n(),
    # Compute centroid of the cluster points (for markers)
    geometry_centroid = st_centroid(st_union(geometry))
  ) %>%
  # Transform back to WGS84 for Leaflet
  st_transform(4326)

# 5. Calculate CONVEX HULLS (Polygons) for visualization
crime_clusters_polygons <- high_harm_events %>%
  filter(cluster_id > 0) %>%
  group_by(cluster_id) %>%
  # Use st_convex_hull to get the boundary of the cluster
  summarise(geometry = st_convex_hull(st_union(geometry))) %>%
  # Join statistics back to the polygons
  left_join(st_drop_geometry(crime_clusters_stats), by = "cluster_id") %>%
  st_transform(4326) # Transform back to WGS84

# 6. Identify Wards containing micro-hotspots (Proxy for Town Centres/NTE areas)
# Use the wards already loaded in Section 1
# FIX: The column for Ward Name is WD24NM, not NAME.
hotspot_wards_sf <- target_wards_sf %>%
  st_filter(st_transform(crime_clusters_polygons, 27700), .predicate = st_intersects) %>%
  # Select only the WD24NM (Ward Name) and the geometry, and transform to WGS84 for Leaflet
  select(WD24NM) %>%
  st_transform(4326)

print("--- IDENTIFIED HIGH-HARM CLUSTERS (DBSCAN) ---")
# Print a structured view of the top 5 clusters by harm
crime_clusters_stats %>%
  st_drop_geometry() %>%
  arrange(desc(Total_Cluster_Harm)) %>%
  mutate(Average_Harm_Per_Incident = round(Total_Cluster_Harm / Incidents, 1)) %>%
  select(cluster_id, Incidents, Total_Cluster_Harm, Average_Harm_Per_Incident) %>%
  head(5) %>%
  print()


# --- 8. VISUALIZATION 3: INTERACTIVE LEAFLET MAP ---

# Create custom palette for high harm individual crimes
pal <- colorFactor(palette = c("orange", "red", "darkred", "purple"), 
                   domain = high_harm_events$`Crime type`)

m <- leaflet(high_harm_events) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # 1. NEW LAYER: Highlight Wards containing micro-hotspots (Proxy for Town Centres/NTE)
  addPolygons(
    data = hotspot_wards_sf,
    fillColor = "yellow",
    fillOpacity = 0.1,
    color = "yellow",
    weight = 1,
    # FIX: Use the corrected column name WD24NM
    popup = ~paste0("<b>Ward:</b> ", WD24NM, "<br>",
                    "<b>Contains:</b> DBSCAN Micro-Hotspot"),
    group = "Micro-Hotspot Wards (Town Centre Proxy)"
  ) %>%
  
  # 2. Heatmap layer (Overall density of high-harm events)
  addHeatmap(
    lng = ~st_coordinates(high_harm_events)[,1],
    lat = ~st_coordinates(high_harm_events)[,2],
    intensity = ~severity, # Heatmap driven by SEVERITY
    blur = 20, max = 0.05, radius = 15,
    group = "Overall High Harm Heatmap"
  ) %>%
  
  # 3. DBSCAN Cluster Polygons (The boundary of the micro-hotspot)
  addPolygons(
    data = crime_clusters_polygons,
    fillColor = "purple",
    fillOpacity = 0.2,
    color = "purple",
    weight = 2,
    popup = ~paste0(
      "<b>Cluster ID:</b> ", cluster_id, "<br>",
      "<b>Total Harm:</b> ", Total_Cluster_Harm, " Days<br>",
      "<b>Incidents:</b> ", Incidents, "<br>",
      "<b>Type:</b> DBSCAN Micro-Hotspot Boundary"
    ),
    group = "Harm Cluster Boundaries"
  ) %>%
  
  # 4. Individual Crime Markers
  addCircleMarkers(
    radius = ~log(severity) * 1.5, # Size based on harm
    color = ~pal(`Crime type`),
    stroke = FALSE, fillOpacity = 0.7,
    popup = ~paste0(
      "<b>Type:</b> ", `Crime type`, "<br>",
      "<b>Date:</b> ", Month, "<br>",
      "<b>Harm Score (Days):</b> ", severity, "<br>",
      "<b>Location:</b> ", Location
    ),
    group = "Individual Crimes"
  ) %>%
  
  addLegend("bottomright", pal = pal, values = ~`Crime type`,
            title = "High Harm Offenses") %>%
  
  addLayersControl(
    overlayGroups = c("Overall High Harm Heatmap", "Individual Crimes", "Harm Cluster Boundaries", "Micro-Hotspot Wards (Town Centre Proxy)"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save the map
saveWidget(m, "interactive_harm_map.html", selfcontained = TRUE)
message("Interactive map saved as 'interactive_harm_map.html")