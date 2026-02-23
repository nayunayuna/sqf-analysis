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

Assignment 2 transforms raw, messy SQF data into a clean, standardized format. Instead of repeating recoding logic 7 times (once per year), we created reusable helper functions that ensure consistent transformation rules across all years (2006-2012).

**Key Improvement:** Reduced from ~250 lines of repetitive code to ~50 lines of modular, reusable functions.

---

### Data Processing Pipeline

The pipeline consists of three scripts executed in order:

#### **1. Load Raw Data** (`scripts/01-load-data.R`)
- Loads individual CSV files for each year (2006-2012) from `data/raw/`
- Combines all years into a single tibble
- Adds `data_year` column for year tracking
- **Output:** `data/sqf_raw.rds` (3.9M rows × 124 columns)
- ⚠️ *Run this only once* when you have new raw data

#### **2. Recode Data** (`scripts/02-recode-data.R`)
- Loads raw data from `data/sqf_raw.rds`
- Applies recoding transformations to each year
- Standardizes column names, types, and values
- **Output:** `data/sqf_clean.rds` (3.9M rows × 11 columns)
- **Processing time:** ~2-3 minutes for full dataset

#### **3. Validate Data** (`scripts/03-validate-data.R`)
- Loads cleaned data from `data/sqf_clean.rds`
- Runs 11 comprehensive validation checks
- Generates formatted validation report
- **Output:** `output/validation_report.rds`

---

### Helper Functions

All helper functions are defined in `R/data_recoding.R`:

#### **`recode_race(race_raw)`**
Converts NYPD single-letter race codes to standardized category names.

**Mapping:**
- `W` → White
- `B` → Black
- `P`, `Q` → Hispanic
- `A` → Asian
- `I`, `Z` → Other
- Invalid codes → NA

**Output:** Ordered factor with levels: White < Black < Hispanic < Asian < Other

```r
recode_race(c("W", "B", "P", "Q", "X"))
# [1] White Black Hispanic Hispanic <NA>
# Levels: White < Black < Hispanic < Asian < Other