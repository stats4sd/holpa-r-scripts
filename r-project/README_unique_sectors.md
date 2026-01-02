# Getting Unique Sector Values from main_surveys

This directory contains scripts to extract unique/distinct sector values from the `main_surveys` dataset.

## Quick Start

### Option 1: Using the provided script

Run the following command from the `r-project` directory:

```r
Rscript get_unique_sectors.R
```

This will:
- Load the `main_surveys` data from the database
- Extract unique sector values
- Display the results in the console
- Save the unique sectors to `unique_sectors.csv`
- Save the sector frequency distribution to `sector_frequency.csv`

### Option 2: Interactive R session

```r
# Load required library
library(tidyverse)

# Source the database connection to load main_surveys
source("data_processing/get_db_connection.R")

# Get unique sectors using dplyr
unique_sectors <- main_surveys %>%
  distinct(sector) %>%
  pull(sector)

# Display results
print(unique_sectors)

# Or using base R
unique_sectors <- unique(main_surveys$sector)
```

### Option 3: Using the helper function

```r
# Source the script
source("get_unique_sectors.R")

# Use the function with any data frame that has a 'sector' column
unique_sectors <- get_unique_sectors(main_surveys)
```

## Files

- `get_unique_sectors.R` - Main script to extract unique sectors
- `examples/get_unique_sectors_example.R` - Examples demonstrating different approaches

## Output

The script generates two files:

1. **unique_sectors.csv** - A list of all unique sector values
2. **sector_frequency.csv** - Frequency count of each sector value

## Requirements

- R (version 3.6 or higher recommended)
- tidyverse package
- Database connection configured (for loading main_surveys)

## Additional Information

The `sector` field in `main_surveys` represents different agricultural sectors such as:
- Agriculture
- Livestock
- Fisheries
- Mixed systems
- etc.

The exact values depend on your dataset.
