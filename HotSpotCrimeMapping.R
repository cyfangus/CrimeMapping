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
library(forecast)
library(spatstat)

# Prepare council boundary for Kingston and Sutton boroughs----
zip_url <- "https://data.london.gov.uk/download/38460723-837c-44ec-b9f0-1ebe939de89a/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip"
download.file(zip_url, destfile = "london_boundaries.zip", mode = "wb")
unzip("london_boundaries.zip", exdir = "london_boundaries")
shp_path <- file.path("london_boundaries", "statistical-gis-boundaries-london", "ESRI")

target_boroughs <- c("Kingston upon Thames", "Sutton")

lsoa_sf <- st_read(file.path(shp_path, "LSOA_2011_London_gen_MHW.shp"))
lsoa_target_sf <- lsoa_sf |>
  filter(LAD11NM %in% target_boroughs) |>
  st_transform(27700)

boroughs_sf <- st_read(file.path(shp_path, "London_Borough_Excluding_MHW.shp"))
target_sf <- boroughs_sf |>
  filter(NAME %in% target_boroughs) |>
  st_transform(27700)

target_boundary <- st_union(target_sf)
target_boundary_sf <- st_sf(geometry = target_boundary)

unlink("london_boundaries.zip")
unlink("london_boundaries", recursive = TRUE)

# Prepare ward boundary for Kingston and Sutton boroughs----
# API endpoint for all ward boundaries as GeoJSON
ward_geojson_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_May_2024_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

wards_sf <- st_read(ward_geojson_url) |>
  st_transform(27700)

wards_centroids <- wards_sf |>
  st_make_valid() |>
  st_centroid() |>
  st_transform(27700)

# Determine which centroids fall within borough_sf
centroids_in_borough <- st_within(wards_centroids, target_boundary_sf, sparse = FALSE)[,1]

# Filter wards where centroid is inside borough polygons
target_wards_sf <- wards_sf[centroids_in_borough, ]

# Prepare street-level crime data----
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
  crime_sf <- st_as_sf(crime_data_clean, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
    st_transform(27700)
  
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
crime_sf <- map(
  latest_36_months_chr,
  possibly(~ extract_data(.x, download_dir, target_boundary_sf), otherwise = NULL)) |>
  compact() |>
  bind_rows()

# Crime situation overview in the boroughs----
crime_type_summary <- crime_sf |>
  st_drop_geometry() |>
  count(`Crime type`, name = "count")|>
  arrange(desc(count))

# Bar chart
crime_type_summary_barchart <- crime_type_summary |>
  mutate(
    type = fct_reorder(`Crime type`, count),
    colour = case_when(
      row_number() <= 2 ~ "red",
      TRUE ~ "grey40"
    )
  ) |>
  ggplot(
    aes(x = count, y = type, fill = colour, label = count)
  ) +
  geom_col() +
  geom_text(colour = "black", fontface = "bold", hjust = 0, size = 2.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30000)) +
  labs(
    title = str_glue(
      "Number of Reported Crime by Types in Kingston and Sutton in the past 36 months"
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

print(crime_type_summary_barchart)

# Time series visualisation----
crime_time_series <- crime_sf |>
  st_drop_geometry() |>
  mutate(
    MonthDate = lubridate::ym(Month)
  ) |>
  group_by(MonthDate, `Crime type`) |>
  summarise(count = n(), .groups = 'drop') |>
  tidyr::replace_na(list(count = 0))

crime_time_series <- crime_time_series |>
  mutate(
    plot_group = ifelse(`Crime type` %in% c("Violence and sexual offences", "Anti-social behaviour"), "High frequency", "Other")
  )

ggplot(crime_time_series, aes(x = MonthDate, y = count, color = `Crime type`, group = `Crime type`)) +
  geom_line(size = 1) +
  geom_point(size = 1.3) +
  facet_wrap(~ plot_group, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Monthly Crime Trends by Type (Sutton & Kingston, last 36 months)",
    subtitle = "Top crime types separated for clarity.",
    x = "Month",
    y = "Monthly Crime Count",
    color = "Crime Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 15)
  )

# Mapping violent and sexual crime----
# Isolate the violent or sexual crime data
violent_or_sexual <- crime_sf |>
  filter(`Crime type` == "Violence and sexual offences") |>
  mutate(
    Month = lubridate::ym(Month),
    Quarter = paste0(lubridate::year(Month), " Q", lubridate::quarter(Month))
  ) |>
  st_transform(27700)

v_or_s_gi_quarterly <- violent_or_sexual |>
  group_by(Quarter) |>
  group_modify(~ {
    .x |>
      hotspot_gistar(cell_size = 200, bandwidth_adjust = 0.25, quiet = FALSE) |>
      filter(gistar > 0, pvalue < 0.05) |>
      st_intersection(target_wards_sf)
  }) |>
  ungroup() |>
  st_as_sf()

v_or_s_gi_quarterly <- v_or_s_gi_quarterly |>
  mutate(
    Year = as.integer(substr(Quarter, 1, 4)),
    Qtr = as.integer(gsub(".*Q(\\d)", "\\1", Quarter)),
    Date = make_date(Year, (Qtr - 1) * 3 + 1, 1)
  ) |>
  arrange(Date) |>
  mutate(
    QuarterFactor = factor(Quarter, levels = unique(Quarter))
  )

animate_quarterly_map <- ggplot(v_or_s_gi_quarterly) +
  geom_sf(aes(fill = kde), color = NA, alpha = 0.8) +
  geom_sf(data = target_wards_sf, fill = NA, color = "black") +
  geom_sf_text(data = target_wards_sf, aes(label = WD24NM), size = 3, color = "grey50") +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       na.value = "white", 
                       name = "Hotspot intensity (kde)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Violent or Sexual Crime Hotspot Maps: {closest_state}",
    subtitle = "Gi* statistic significant hotspots by quarter",
    caption = "Note: Only significant hotspots shown"
  ) +
  transition_states(Quarter, transition_length = 2, state_length = 1, wrap = FALSE) +
  ease_aes('linear')

animate(animate_quarterly_map,
        nframes = length(unique(v_or_s_gi_quarterly$Quarter)) * 10,
        fps = 5,
        width = 800, height = 600)

anim_save("quarterly_crime_hotspots.gif")


# Total crime counts across all types by month
total_crime_ts <- crime_time_series %>%
  group_by(MonthDate) %>%
  summarise(total_count = sum(count))

ggplot(total_crime_ts, aes(x = MonthDate, y = total_count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(title = "Monthly Crime Counts Over Time",
       x = "Month",
       y = "Crime Count") +
  theme_minimal()

# Crime situation overview by wards----
crime_by_wards <- st_join(crime_sf, target_wards_sf["WD24NM"], left = TRUE) |>
  st_drop_geometry() |>
  group_by(WD24NM) |>
  summarise(count = n()) |>
  arrange(desc(count))

crime_by_wards_barchart <- head(crime_by_wards, 20) |>
  mutate(
    type = fct_reorder(`WD24NM`, count),
    colour = case_when(
      row_number() <= 2 ~ "red",
      TRUE ~ "grey40"
    )
  ) |> 
  ggplot(
    aes(x = count, y = type, fill = colour, label = count)
  ) +
  geom_col() +
  geom_text(colour = "black", fontface = "bold", hjust = 0, size = 2.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15000)) +
  labs(
    title = str_glue(
      "Top 20 Wards by Crime Counts in the past 36 months"
    ),
    x = "Crime incidents count",
    y = "Wards"
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

print(crime_by_wards_barchart)
