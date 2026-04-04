# sqf-analysis
## ==== Assignment #1: Setting up Files ====

# NYC Stop and Frisk Analysis (2006-2012)
Analysis of NYPD Stop, Question, and Frisk data.

## Data Source

Data from [NYPD Stop, Question and Frisk Database](https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page)

## Setup

1. Download 2006-2012 data files and place in `data/raw/`
2. Install required packages: `tidyverse`, `lubridate`
3. Run `scripts/01-load-data.R`

## Required Packages

- tidyverse
- lubridate

## File structure
project/
├── data/
│   ├── raw/
│   │   ├── 2006.csv
│   │   ├── 2007.csv
│   │   └── ... (2008-2012)
│   ├── sqf_raw.rds          # Output of 01-load-data.R
│   └── sqf_clean.rds        # Output of 02-recode-data.R
├── output/
│   └── validation_report.rds # Output of 03-validate-data.R
├── R/
│   ├── data_loading.R       # Assignment 1: Load functions
│   └── data_recoding.R      # Assignment 2: Recode functions + validation
├── scripts/
│   ├── 01-load-data.R       # Load raw data
│   ├── 02-recode-data.R     # Recode to clean format
│   └── 03-validate-data.R   # Validate clean data
└── README.md

## ==== Assignment 2: Data Recoding & Validation ====
### Overview

Assignment 2 transforms raw, messy SQF data into a clean, standardized format. Instead of repeating recoding logic 7 times (once per year), we created reusable helper functions that ensure consistent transformation rules across all years (2006-2012). Reduced from ~250 lines of repetitive code to ~50 lines of modular, reusable functions.

---

### Data Processing Pipeline

The pipeline consists of three scripts executed in order:

#### **1. Load Raw Data** (`scripts/01-load-data.R`)
- Loads individual CSV files for each year (2006-2012) from `data/raw/`
- Combines all years into a single tibble
- Adds `data_year` column for year tracking
- **Output:** `data/sqf_raw.rds` (3.9M rows × 124 columns)
-  *Run this only once* when you have new raw data


#### **2. Recode Data** (`scripts/02-recode-data.R`)
- Loads raw data from `data/sqf_raw.rds`
- Applies recoding transformations to each year
- Standardizes column names, types, and values
- **Output:** `data/sqf_clean.rds` (3.9M rows × 11 columns)


#### **3. Validate Data** (`scripts/03-validate-data.R`)
- Loads cleaned data from `data/sqf_clean.rds`
- Runs 11 comprehensive validation checks
- Generates formatted validation report
- **Output:** `output/validation_report.rds`

---

## ==== Assignment #4: Spatial Analysis & Mapping ====
Assignment 4 geocodes Stop-and-Frisk data to NYC Census tract boundaries using spatial joins, then aggregates and visualizes geographic patterns in stops and force use. We created reusable spatial functions to download Census boundaries, convert coordinates to spatial objects, and perform the geocoding pipeline. Results are mapped as choropleth visualizations showing disparities across neighborhoods.

## Required Packages

- "sf"
- "tigris"

----

### Data Processing Pipeline
The pipeline consists of this scripts executed following scripts 1 to 3:

### 4. Geocode Data (scripts/04-geocode-data.R)
# Geocode SQF data using spatial functions
- Downloads Census tract boundaries for all five NYC boroughs via tigris
- Converts SQF coordinates (xcoord, ycoord) to spatial point objects
- Performs spatial join (st_within) to assign each stop to a Census tract
- Validates geocoding results 

**Output:** 
- `data/sqf_geocoded.rds` (3.8M stops with tract assignments)
-`data/nyc_tracts.rds` (2,168 Census tract boundaries)

# Aggregates stop-level data to Census tract level
- Computes: total stops, counts by race, percentage Black, percentage force used
- Groups by year and tract (2006-2012)
 **Output:** `data/tract_by_year.rds` (15,080 rows: 2,158 tracts × 7 years)

# Create Maps 
- Joins aggregated data back to tract geometries
- Creates choropleth maps using ggplot2 + geom_sf()
- Map 1: Total stops by Census tract (2010)
- Map 2: Percentage of stops involving Black civilians (2010)

**Outputs:**
- `output/map_total_stops_2010.png`
- `output/map_pct_black_2010.png`

----

### Helper Functions
Created in `R/spatial_functions.R`

- `get_nyc_tracts(year, crs)` - Download and prepare Census tract boundaries for five NYC counties
- `make_spatial(data, crs)` - Convert data frame with xcoord/ycoord to spatial points
- `spatial_join(points, polygons)` - Join spatial points to polygons with validation
- `aggregate_by_tract(geocoded_data)` - Aggregate stops to tract level with summary statistics

----
