### ======= Part 1: Spatial Helper Functions ======= ###
### 1.1 Download and Prepare Census Tracts
## Write a function that downloads Census tract geometries for NYC 
## and returns a clean `sf` object:


#' Download and prepare NYC Census tract boundaries
#'
#' Downloads tract shapefiles from the US Census Bureau via the tigris
#' package and standardizes column names. NYC spans five counties
#' (Manhattan=061, Brooklyn=047, Queens=081, Bronx=005, Staten Island=085).
#'
#' @param year Integer, Census year for tract boundaries (default: 2010)
#' @param crs Integer, EPSG code for coordinate reference system (default: 2263)
#' @return sf object with columns: ct_code, area_land, area_water, geometry
#'
#' @examples
#' tracts <- get_nyc_tracts()
#' plot(st_geometry(tracts))


get_nyc_tracts <- function(year = 2010, crs = 2263) {
  
  # NYC county FIPS codes
  nyc_counties <- c(
    "061",  # Manhattan
    "047",  # Brooklyn
    "081",  # Queens
    "005",  # Bronx
    "085"   # Staten Island
  )
  
  # Download tracts for each county and combine
  tracts <- map_df(
    nyc_counties,
    ~tigris::tracts(state = "NY", county = .x, year = year, class = "sf")
  )
  
  # Rename columns, select only needed columns, and project CRS
  tracts %>%
    rename(
      ct_code = GEOID10,
      area_land = ALAND10,
      area_water = AWATER10
    ) %>%
    select(ct_code, area_land, area_water, geometry) %>%
    st_transform(crs = crs)
}



### =======  1.2 Convert Data Frame to Spatial Points ======= ###
## Write a function that converts your cleaned SQF data 
## (with `xcoord`/`ycoord` columns) into an `sf` point object:

#' Convert SQF data to spatial points
#'
#' Takes a data frame with xcoord and ycoord columns and converts it
#' to an sf POINT object. Rows with missing coordinates are dropped.
#'
#' @param data Tibble with xcoord and ycoord columns
#' @param crs Integer, EPSG code matching the coordinate system (default: 2263)
#' @return sf object with POINT geometry
#'
#' @examples
#' sqf_spatial <- make_spatial(sqf_clean)


make_spatial <- function(data, crs = 2263) {
  
  # Check that columns exist
  if (!all(c("xcoord", "ycoord") %in% colnames(data))) {
    stop("Data must contain 'xcoord' and 'ycoord' columns")
  }
  
  # Count rows before filtering
  rows_before <- nrow(data)
  
  # Filter to rows with both coordinates present, convert to sf
  result <- data %>%
    filter(!is.na(xcoord) & !is.na(ycoord)) %>%
    st_as_sf(coords = c("xcoord", "ycoord"), crs = crs)
  
  # Report how many rows were dropped
  rows_dropped <- rows_before - nrow(result)
  message(sprintf(
    "Dropped %s rows with missing coordinates (%0.1f%%)",
    format(rows_dropped, big.mark = ","),
    100 * rows_dropped / rows_before
  ))
  
  result
}


### =======  1.3 Spatial Join Function ======= ###
## Write a function that joins spatial points to polygons:


#' Join spatial points to polygons
#'
#' Performs a spatial join to determine which polygon (e.g., Census tract)
#' each point falls within. Points that don't fall within any polygon
#' are dropped.
#'
#' @param points sf object with POINT geometry (e.g., SQF stops)
#' @param polygons sf object with POLYGON geometry (e.g., Census tracts)
#' @return sf object with point data joined to polygon attributes
#'
#' @examples
#' sqf_geocoded <- spatial_join(sqf_spatial, tracts)


spatial_join <- function(points, polygons) {
  
  # Validate inputs are sf objects
  if (!inherits(points, "sf")) stop("points must be an sf object")
  if (!inherits(polygons, "sf")) stop("polygons must be an sf object")
  
  # Check CRS match
  if (st_crs(points) != st_crs(polygons)) {
    stop("points and polygons must have the same CRS")
  }
  
  # Count points before join
  points_before <- nrow(points)
  
  # Perform spatial join
  result <- st_join(points, polygons, join = st_within)
  
  # Count matched points
  points_matched <- sum(!is.na(result$ct_code))
  points_unmatched <- points_before - points_matched
  
  # Report results
  message(sprintf(
    "Spatial join: %s matched, %s unmatched (%0.1f%% coverage)",
    format(points_matched, big.mark = ","),
    format(points_unmatched, big.mark = ","),
    100 * points_matched / points_before
  ))
  
  # Drop unmatched points
  result %>% filter(!is.na(ct_code))
}