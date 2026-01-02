library(tidyverse)

################################################################################
# EXAMPLE: How to get unique sector values from main_surveys
################################################################################

# Source the get_unique_sectors script
source("get_unique_sectors.R")

# Example 1: Using the main database connection
# (This will automatically run when you source get_unique_sectors.R)
cat("=== Example 1: Get unique sectors from database ===\n")
cat("Simply run: Rscript get_unique_sectors.R\n\n")

# Example 2: Using the function with a custom data frame
cat("=== Example 2: Using the get_unique_sectors() function ===\n")

# Create sample data for demonstration
sample_data <- tibble(
  farm_id = 1:10,
  sector = c("Agriculture", "Agriculture", "Livestock", 
             "Fisheries", "Agriculture", "Livestock",
             "Fisheries", "Mixed", "Mixed", "Agriculture")
)

cat("Sample data:\n")
print(sample_data)

# Get unique sectors from sample data
unique_sectors_sample <- get_unique_sectors(sample_data)

cat("\nUnique sectors in sample data:\n")
print(unique_sectors_sample)

# Example 3: Using standard R/tidyverse functions directly
cat("\n=== Example 3: Using dplyr directly ===\n")
cat("You can also use dplyr's distinct() function directly:\n\n")

cat("# Get unique sectors\n")
cat("unique_sectors <- main_surveys %>%\n")
cat("  distinct(sector) %>%\n")
cat("  pull(sector)\n\n")

cat("# Or using base R\n")
cat("unique_sectors <- unique(main_surveys$sector)\n\n")

cat("# Get frequency count\n")
cat("main_surveys %>%\n")
cat("  count(sector, name = 'count') %>%\n")
cat("  arrange(desc(count))\n")
