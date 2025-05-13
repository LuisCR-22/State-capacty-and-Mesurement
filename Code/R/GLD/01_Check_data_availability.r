# Author: Luis Castellanos - Stats Team LAC
# Date: 2025-05-12


rm(list = ls())
cat("\014")  # Clear console


# Set the directory path
dir_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Shared/FY2025/Technology and Inequality project/Raw data/GLD harmonization"

# List all .dta files in the directory
dta_files <- list.files(dir_path, pattern = "\\.dta$", full.names = FALSE)

# Extract the country codes (first 3 letters) from each filename
country_codes <- substr(dta_files, 1, 3)

# Extract the years from each filename
# Pattern appears to be COUNTRY_YEAR_...
years <- numeric(length(dta_files))
for (i in 1:length(dta_files)) {
  # Extract year (4 digits after first underscore)
  year_string <- substr(dta_files[i], 5, 8)
  years[i] <- as.numeric(year_string)
}

# Get unique country codes
unique_countries <- unique(country_codes)
num_countries <- length(unique_countries)

# Create a data frame to analyze years by country
country_years <- data.frame(country = country_codes, year = years)

# Print results
cat("Number of unique countries:", num_countries, "\n")
cat("Country codes:", paste(unique_countries, collapse = ", "), "\n\n")

# Print year range for all countries combined
cat("Overall year range:", min(years, na.rm = TRUE), "to", max(years, na.rm = TRUE), "\n\n")


