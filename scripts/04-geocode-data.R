# ============================================================================
# R/04-geocode-data.R
# 2.1 Geocode SQF data using spatial functions
# ============================================================================

library(tidyverse)
library(sf)
library(tigris)
source("R/spatial_functions.R")

# --- Load cleaned data ---
sqf_clean <- read_rds("data/sqf_clean.rds")

# --- Download Census tract boundaries ---
message("Downloading Census tract boundaries...")
tracts <- get_nyc_tracts()

# --- Convert SQF data to spatial points ---
message("Converting SQF data to spatial points...")
sqf_spatial <- make_spatial(sqf_clean)

# --- Spatial join -- assign each stop to a Census tract ---
message("Performing spatial join...")
sqf_geocoded <- spatial_join(sqf_spatial, tracts)

# --- Print summary ---
message(sprintf(
  "\nGeocoding complete: %s of %s stops (%0.1f%%)",
  format(nrow(sqf_geocoded), big.mark = ","),
  format(nrow(sqf_clean), big.mark = ","),
  100 * nrow(sqf_geocoded) / nrow(sqf_clean)
))

# --- Save results ---
write_rds(sqf_geocoded, "data/sqf_geocoded.rds", compress = "gz")
write_rds(tracts, "data/nyc_tracts.rds", compress = "gz")

message("Results saved to data/sqf_geocoded.rds and data/nyc_tracts.rds")

# ============================================================================
# 2.2 Validation Checks
# ============================================================================

# --- How many stops fell outside NYC? ---
message("Check 1: Stops outside NYC boundaries")
stops_with_coords <- sqf_clean %>%
  filter(!is.na(xcoord) & !is.na(ycoord))

stops_outside <- nrow(stops_with_coords) - nrow(sqf_geocoded)

message(sprintf(
  "  %s stops fell outside NYC boundaries (%0.2f%%)",
  format(stops_outside, big.mark = ","),
  100 * stops_outside / nrow(stops_with_coords)
))

# --- Compare geocoded vs original data ----
message("\nCheck 3: Data integrity")
message(sprintf(
  "  Original SQF records: %s",
  format(nrow(sqf_clean), big.mark = ",")
))
message(sprintf(
  "  Records with coordinates: %s",
  format(nrow(stops_with_coords), big.mark = ",")
))
message(sprintf(
  "  Successfully geocoded: %s",
  format(nrow(sqf_geocoded), big.mark = ",")
))

message("\n")


# ============================================================================
## 3. Aggregation and Mapping
# ============================================================================

### 3.1 Aggregate Stops by Census Tract
# Write a function that aggregates stop-level data to the tract level:


#' Aggregate SQF stops by Census tract
#'
#' Computes summary statistics for each Census tract from
#' geocoded stop-level data.
#'
#' @param geocoded_data sf object, geocoded SQF data (from spatial_join)
#' @return Tibble with one row per tract: ct_code, total_stops,
#'   stops_black, stops_hispanic, stops_white, pct_black, pct_force
#'
#' @examples
#' tract_summary <- aggregate_by_tract(sqf_geocoded)


aggregate_by_tract <- function(geocoded_data) {
  
  geocoded_data %>%
    st_set_geometry(NULL) %>%
    group_by(ct_code) %>%
    summarize(
      total_stops = n(),
      stops_black = sum(race == "Black", na.rm = TRUE),
      stops_hispanic = sum(race == "Hispanic", na.rm = TRUE),
      stops_white = sum(race == "White", na.rm = TRUE),
      stops_asian = sum(race == "Asian", na.rm = TRUE),
      stops_other = sum(race == "Other", na.rm = TRUE),
      pct_black = 100 * stops_black / total_stops,
      pct_hispanic = 100 * stops_hispanic / total_stops,
      pct_force = 100 * sum(police_force == 1, na.rm = TRUE) / total_stops,
      .groups = "drop"
    )
}

# ============================================================================
## 3.2 Use `map()` for Multiple Aggregations Aggregate Stops by Census Tract
# ============================================================================

# Write a function that aggregates stop-level data to the tract level:

tract_by_year <- sqf_geocoded %>%
  st_set_geometry(NULL) %>%
  group_by(year, ct_code) %>%
  summarize(
    total_stops = n(),
    stops_black = sum(race == "Black", na.rm = TRUE),
    stops_hispanic = sum(race == "Hispanic", na.rm = TRUE),
    stops_white = sum(race == "White", na.rm = TRUE),
    stops_asian = sum(race == "Asian", na.rm = TRUE),
    stops_other = sum(race == "Other", na.rm = TRUE),
    pct_black = 100 * stops_black / total_stops,
    pct_hispanic = 100 * stops_hispanic / total_stops,
    pct_force = 100 * sum(police_force == 1, na.rm = TRUE) / total_stops,
    .groups = "drop"
  )



# ============================================================================
## 3.3 Create Maps
# ============================================================================

#' Create a choropleth map of SQF data by Census tract
#'
#' @param tracts sf object with tract geometries and summary data
#' @param fill_var Character, name of the variable to map
#' @param title Character, plot title
#' @return ggplot object


# --- Select 2010 data for mapping ---
tract_2010 <- tract_by_year %>%
  filter(year == 2010)

# --- Join aggregated data back to tract geometries ---
tracts_mapped <- tracts %>%
  left_join(tract_2010, by = "ct_code")

# --- MAP 1: Total stops by Census tract ---
map1 <- ggplot(tracts_mapped) +
  geom_sf(aes(fill = total_stops), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Total Stops", trans = "log10") +
  labs(
    title = "Stop-and-Frisk Stops by NYC Census Tract (2010)",
    subtitle = "Log scale (darker = more stops)"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/map_total_stops_2010.png", map1, width = 10, height = 10, dpi = 300)
message("Map 1 saved: output/map_total_stops_2010.png")

# --- MAP 2: Percent of stops involving Black civilians ---
map2 <- ggplot(tracts_mapped) +
  geom_sf(aes(fill = pct_black), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "% Black Stops", option = "plasma") +
  labs(
    title = "Percentage of Stops Involving Black Civilians by Census Tract (2010)",
    subtitle = "Darker = higher percentage"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/map_pct_black_2010.png", map2, width = 10, height = 10, dpi = 300)
message("Map 2 saved: output/map_pct_black_2010.png")

# --- Display maps ---
print(map1)
print(map2)