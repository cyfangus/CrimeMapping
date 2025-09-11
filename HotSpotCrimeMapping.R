library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(dplyr)
library(httr)
library(readr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(sfhotspot)
library(ggspatial)
library(lubridate)
library(zoo)
library(tsibble)
library(progressr)
library(fable)
library(gganimate)
library(gifski)
library(magick)

# Download borough boundaries and filter for South West Basic Command Unit (SWBCU) boroughs----
zip_url <- "https://data.london.gov.uk/download/38460723-837c-44ec-b9f0-1ebe939de89a/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip"
download.file(zip_url, destfile = "london_boundaries.zip", mode = "wb")
unzip("london_boundaries.zip", exdir = "london_boundaries")
shp_path <- file.path("london_boundaries", "statistical-gis-boundaries-london", "ESRI")

swbcu_boroughs <- c("Richmond upon Thames", "Kingston upon Thames", "Merton", "Wandsworth") 

lsoa_sf <- st_read(file.path(shp_path, "LSOA_2011_London_gen_MHW.shp"))
lsoa_swbcu_sf <- lsoa_sf |>
  filter(LAD11NM %in% swbcu_boroughs) |>
  st_transform(27700)

boroughs_sf <- st_read(file.path(shp_path, "London_Borough_Excluding_MHW.shp"))
swbcu_sf <- boroughs_sf |>
  filter(NAME %in% swbcu_boroughs) |>
  st_transform(27700)

swbcu_boundary <- st_union(swbcu_sf)
swbcu_boundary_sf <- st_sf(geometry = swbcu_boundary)

unlink("london_boundaries.zip")
unlink("london_boundaries", recursive = TRUE)

# Download street-level crime data inside MPS and filter----
download_dir <- tempdir()
latest_url <- "https://data.police.uk/data/archive/latest.zip"
latest_zip <- file.path(download_dir, "latest.zip")

resp <- httr::GET(latest_url, httr::write_disk(latest_zip, overwrite = TRUE), httr::progress())
httr::stop_for_status(resp)
utils::unzip(latest_zip, exdir = download_dir)

extract_data <- function(month, base_dir, boundary_sf) {
  month_dir <- file.path(base_dir, month)
  if (!dir.exists(month_dir)) {
    warning(paste("Month folder not found:", month))
    return(NULL)
  }
  
  # Find Metropolitan street CSV file for the month
  # pattern example: "2024-01-metropolitan-street.csv"
  mps_file <- list.files(month_dir,
                           pattern = paste0("^", month, "-metropolitan-street\\.csv$"),
                           full.names = TRUE)
  
  if (length(mps_file) == 0) {
    warning(paste("MPS street data not found for", month))
    return(NULL)
  }
  
  # Read the CSV (adjust guess_max for large files)
  crime_data <- read_csv(mps_file[1], col_types = cols(), guess_max = 1e6)
  
  # Filter out rows with missing coordinates
  crime_data_clean <- crime_data |>
    filter(!is.na(Longitude) & !is.na(Latitude))
  
  if (nrow(crime_data_clean) == 0) {
    warning(paste("No valid coordinates found in data for", month))
    return(NULL)
  }
  
  # Convert to sf points
  crime_sf <- st_as_sf(crime_data_clean, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  crime_sf <- st_transform(crime_sf, 27700)
  
  # Spatial filter with SWBCU boundary
  crime_filtered <- st_join(crime_sf, boundary_sf, join = st_within, left = FALSE)
  
  return(crime_filtered)
}

folders <- list.dirs(path = download_dir, full.names = FALSE, recursive = FALSE)
month_folders <- folders[grepl("^\\d{4}-\\d{2}$", folders)]
month_folders_dates <- ymd(paste0(month_folders, "-01"))
latest_36_months <- sort(month_folders_dates, decreasing = TRUE)[1:36]
latest_36_months_chr <- format(latest_36_months, "%Y-%m")

# Now map the function over last 12 months only
all_swbcu_crime_sf <- map(
  latest_36_months_chr,
  possibly(~ extract_data(.x, download_dir, swbcu_boundary_sf), otherwise = NULL)) |>
  compact() |>
  bind_rows()

# Which types of crime should the West BCU tasking team be focused on?----
crime_type_summary <- all_swbcu_crime_sf |>
  st_drop_geometry() |>
  count(`Crime type`, name = "count")|>
  arrange(desc(count))


# Bar chart
crime_type_summary |>
  mutate(
    type = fct_reorder(`Crime type`, count),
    colour = case_when(
      row_number() <= 3 ~ "red",
      TRUE ~ "grey40"
    )
  ) |>
  ggplot(
    aes(x = count, y = type, fill = colour, label = count)
  ) +
  geom_col() +
  geom_text(colour = "black", fontface = "bold", hjust = 0, size = 2.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 60000)) +
  labs(
    title = str_glue(
      "Number of Reported Crime by Types in South West BCU in the past 36 months"
    ),
    x = "Crime incidents count",
    y = "Crime type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(
      colour = "grey30",
      face = "bold",
      size = 13
    )
  ) +
  scale_fill_identity()

# Which parts of the BCU should the team focus on patrolling to prevent violent and sexual crime?----
# Isolate the violent or sexual crime data
violent_or_sexual <- all_swbcu_crime_sf |>
  filter(`Crime type` == "Violence and sexual offences") |>
  mutate(
    Month = lubridate::ym(Month),
    Quarter = paste0(lubridate::year(Month), " Q", lubridate::quarter(Month))
  ) |>
  st_transform(27700)

# Calculate Gi* statistic (filter for only significant hotspot cells)
v_or_s_gi_quarterly <- violent_or_sexual |>
  group_by(Quarter) |>
  group_modify(~ {
    .x |>
      hotspot_gistar(cell_size = 200, bandwidth_adjust = 0.25, quiet = FALSE) |>
      filter(gistar > 0, pvalue < 0.05) |>
      st_intersection(swbcu_boundary_sf)
  }) |>
  ungroup() |>
  st_as_sf()

# Plot every quarter
v_or_s_gi_quarterly$QuarterFactor <- factor(
  v_or_s_gi_quarterly$Quarter,
  levels = unique(v_or_s_gi_quarterly$Quarter[order(as.Date(paste0(substr(v_or_s_gi_quarterly$Quarter, 1, 4), "-", 
                                                                   (as.integer(substr(v_or_s_gi_quarterly$Quarter, 7, 7)) - 1) * 3 + 1, "-01")))])
)

ggplot(v_or_s_gi_quarterly) +
  geom_sf(aes(fill = kde), color = NA, alpha = 0.8) +
  geom_sf(data = swbcu_boundary_sf, fill = NA, color = "black") +
  geom_sf_text(data = swbcu_sf, aes(label = NAME), size = 3, color = "grey50") +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       na.value = "white", 
                       name = "Hotspot intensity (kde)") +
  facet_wrap(~ QuarterFactor, ncol = 4) +  # Adjust columns as needed
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Quarterly Violent or Sexual Crime Hotspot Maps",
    subtitle = "Gi* statistic significant hotspots by quarter",
    caption = "Note: Only significant hotspots shown"
  )


# SARIMA for six-month prediction per grid----
v_or_s_gi_quarterly <- v_or_s_gi_quarterly |>
  mutate(grid_id = as.character(st_geometry(v_or_s_gi_quarterly)))

grid_geom <- v_or_s_gi_quarterly |>
  select(grid_id, geometry) |>
  distinct(grid_id, .keep_all = TRUE)

# Aggregate kde per grid_id per Month
hotspot_tsibble <- v_or_s_gi_quarterly |>
  st_drop_geometry() |>
  select(grid_id, Quarter, kde) |>
  group_by(grid_id, Quarter) |>
  summarise(kde = mean(kde, na.rm=TRUE), .groups = "drop") |>
  filter(!is.na(kde)) |>
  mutate(QuarterDate = as.Date(paste0(substr(Quarter, 1, 4), "-", 
                                      (as.integer(substr(Quarter, 7,7)) - 1) * 3 + 1, "-01"))) |>
  mutate(Quarter = yearquarter(QuarterDate)) |>
  as_tsibble(index = Quarter, key = grid_id) |>
  fill_gaps(kde = 0)

# Fit SARIMA model (or other) per grid_id
handlers("progress")
with_progress({
  models <- hotspot_tsibble |>
    model(ARIMA(kde))
})

  # Forecast 4 quarters ahead for each grid_id
forecasts <- models |>
  forecast(h = 4)

# Get the name of the index column (likely "Quarter" or "QuarterDate")
index_col <- index_var(hotspot_tsibble)

# Extract the last observation's time value from the index column
last_obs_quarter <- hotspot_tsibble |>
  slice_max(!!sym(index_col), n = 1) |>
  pull(!!sym(index_col)) |>
  max()

# Generate next 4 quarters dates
upcoming_quarters <- seq(from = as.Date(format(last_obs_quarter, "%Y-%m-01")) + 90, length.out = 4, by = "quarter")

# Combine forecasts with spatial grid polygons for mapping and filter to next 4 quarters only
grid_sf_with_forecast <- forecasts |>
  as_tibble() |>
  filter(Quarter > max(hotspot_tsibble$Quarter)) |>
  left_join(grid_geom, by = "grid_id") |>
  st_as_sf() |>
  mutate(
    QuarterLabel = paste0(year(Quarter), " Q", quarter(Quarter)),
    QuarterFactor = factor(QuarterLabel, levels = unique(QuarterLabel[order(Quarter)]))
  )

# Simplify geometry for faster plotting
grid_sf_with_forecast_simple <- st_simplify(grid_sf_with_forecast, dTolerance = 100)

# Plot forecasted 4 upcoming quarters only
ggplot(grid_sf_with_forecast) +
  geom_sf(aes(fill = .mean), color = NA, alpha = 0.8) +
  geom_sf(data = swbcu_boundary_sf, fill = NA, color = "grey50") +
  geom_sf_text(data = swbcu_sf, aes(label = NAME), size = 3, color = "grey50") +
  scale_fill_gradient(
    low = "gray90",   # very light grey for low intensity
    high = "darkred", # deep red for high intensity
    na.value = "white",
    name = "Predicted Hotspot Intensity"
  ) +
  facet_wrap(~ QuarterFactor, ncol = 4) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Predicted Violent or Sexual Crime Hotspots for Next Year",
    subtitle = "Forecasted kernel density estimates from SARIMA models",
    caption = "Note: Hotspots predicted spatially per grid cell"
  )

