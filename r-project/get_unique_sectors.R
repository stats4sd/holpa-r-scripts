library(tidyverse)

################################################################################
# GET UNIQUE SECTOR VALUES FROM main_surveys
################################################################################

# Function to get unique sector values from a data frame
get_unique_sectors <- function(data) {
  # Check if sector column exists
  if (!"sector" %in% colnames(data)) {
    stop("Error: 'sector' column not found in the data")
  }
  
  # Get unique/distinct sector values
  unique_sectors <- data %>%
    distinct(sector) %>%
    pull(sector)
  
  return(unique_sectors)
}

# Main execution
# Try to load main_surveys from database connection
tryCatch({
  source("data_processing/get_db_connection.R")
  
  # Get unique sectors
  unique_sectors <- get_unique_sectors(main_surveys)
  
  # Display the unique sectors
  cat("Unique sector values in main_surveys:\n")
  print(unique_sectors)
  
  # Count of unique sectors
  cat("\nTotal number of unique sectors:", length(unique_sectors), "\n")
  
  # Create a frequency table
  sector_counts <- main_surveys %>%
    count(sector, name = "count") %>%
    arrange(desc(count))
  
  cat("\nSector frequency distribution:\n")
  print(sector_counts)
  
  # Save results to CSV
  output_file <- "unique_sectors.csv"
  write.csv(data.frame(sector = unique_sectors), output_file, row.names = FALSE)
  cat("\nUnique sectors saved to:", output_file, "\n")
  
  # Save frequency table
  freq_file <- "sector_frequency.csv"
  write.csv(sector_counts, freq_file, row.names = FALSE)
  cat("Sector frequency distribution saved to:", freq_file, "\n")
  
}, error = function(e) {
  cat("Note: Could not load main_surveys from database.\n")
  cat("Error message:", conditionMessage(e), "\n")
  cat("\nTo use this script:\n")
  cat("1. Ensure database connection is properly configured, OR\n")
  cat("2. Load your data and call get_unique_sectors(your_data)\n")
})
