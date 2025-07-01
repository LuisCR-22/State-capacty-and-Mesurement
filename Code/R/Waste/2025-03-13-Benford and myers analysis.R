########################################
# Benford Wage Analysis - Updated Version with Myers Index
########################################

# 0. Clean up the workspace and load libraries
rm(list = ls())
gc()

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

# 1. Define file paths for input and outputs
input_path <- "C:/Users/Paula/Desktop/New folder/GLD database"

# Output paths
output_path_excel_modified <- "C:/Users/Paula/Desktop/Output"
output_path_PNG_modified   <- "C:/Users/Paula/Desktop/Output"

# 2. Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# 3. Define the list of country codes to process
country_list <- c("ALB","ARM","BGD","BOL","BRA","CHL","CMR","COL","ETH","GEO","GMB","IDN","IND","LKA","MEX","MNG","NPL","PAK","PHL","RWA","SLE","THA","TUN","TUR","TZA","ZAF","ZMB","ZWE")

# 4. Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      correlation_benford_observed = numeric(),
                      linear_distance = numeric(),
                      quadratic_distance = numeric(),
                      kuiper_stat = numeric(),
                      kuiper_pval = numeric(),
                      ks_stat = numeric(),
                      ks_pval = numeric(),
                      mae = numeric(),
                      index_myers = numeric(),
                      stringsAsFactors = FALSE)

# 5. Define Kuiper Test function
diff_cdf <- function(obs, expected) {
  obs_cdf <- cumsum(obs) / sum(obs)
  expected_cdf <- cumsum(expected) / sum(expected)
  diff <- obs_cdf - expected_cdf
  return(diff)
}

kuiper_test <- function(obs, expected) {
  diff <- diff_cdf(obs, expected)
  V <- max(diff) - min(diff)  # Kuiper statistic
  n <- length(obs)
  p_value <- exp(-2 * n * V^2)
  return(list(statistic = V, p.value = p_value))
}

# 6. Process files
for (country in country_list) {
  
  message("Processing country: ", country)
  
  country_files <- list.files(path = input_path, 
                              pattern = paste0("^", country, ".*\\.dta$"), 
                              full.names = TRUE)
  
  if(length(country_files) == 0) {
    message("  No files found for country ", country)
    next
  }
  
  country_data_list <- list()
  
  for (file in country_files) {
    message("  Processing file: ", file)
    tryCatch({
      dat <- read_dta(file) %>% as.data.table()
      
      required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age")
      if(!all(required_vars %in% names(dat))) {
        message("    Skipping file due to missing required variables.")
        next
      }
      
      dat <- dat[!is.na(wage_no_compen) & unitwage == 5]
      if(nrow(dat) == 0) {
        message("    No observations remain after filtering in this file.")
        next
      }
      
      dat[, first_digit := as.numeric(substr(as.character(abs(wage_no_compen)), 1, 1))]
      dat <- dat[first_digit %in% 1:9]
      
      myers_index <- dat[age <= 60, .N, by = .(age = as.numeric(age) %% 10)]
      myers_index <- myers_index[, sum(abs(N / sum(N) - 0.1)) * 100]
      
      dat[, index_myers := myers_index]
      country_data_list[[file]] <- dat
    }, error = function(e) {
      message("    Error processing file: ", e$message)
    })
  }
  
  if(length(country_data_list) == 0) {
    message("  No valid data found for country ", country)
    next
  }
  
  country_data <- rbindlist(country_data_list, use.names = TRUE, fill = TRUE, ignore.attr = TRUE)
  
  country_years <- unique(country_data$year)
  for (yr in country_years) {
    data_year <- country_data[year == yr]
    digits <- data_year$first_digit
    n <- length(digits)
    if(n < 30) {
      message(paste("  Skipping year", yr, "due to insufficient data (n =", n, ")"))
      next
    }
    
    obs_table <- table(factor(digits, levels = 1:9))
    obs_freq <- as.numeric(obs_table) / n
    
    corr_val <- cor(obs_freq, benford_expected)
    linear_distance <- sum(obs_freq - benford_expected)
    quadratic_distance <- sum((obs_freq - benford_expected)^2)
    
    kuiper_result <- kuiper_test(obs_freq, benford_expected)
    ks_result <- ks.test(obs_freq, benford_expected)
    
    kuiper_stat <- kuiper_result$statistic
    kuiper_pval <- kuiper_result$p.value
    
    ks_stat <- ks_result$statistic
    ks_pval <- ks_result$p.value
    
    mae <- mean(abs(obs_freq - benford_expected))
    
    results <- rbind(results,
                     data.frame(countrycode = country,
                                year = yr,
                                correlation_benford_observed = corr_val,
                                linear_distance = linear_distance,
                                quadratic_distance = quadratic_distance,
                                kuiper_stat = kuiper_stat,
                                kuiper_pval = kuiper_pval,
                                ks_stat = ks_stat,
                                ks_pval = ks_pval,
                                mae = mae,
                                index_myers = unique(data_year$index_myers),
                                stringsAsFactors = FALSE))
    
  }
}

# 7. Create a scatter plot for each test
scatter_linear <- ggplot(results, aes(x = index_myers, y = linear_distance, color = as.factor(year))) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "blue") +
  labs(title = "Scatter Plot: Myers Index vs Linear Distance",
       x = "Myers Index", y = "Linear Distance", color = "Year") +
  theme_minimal()

scatter_quadratic <- ggplot(results, aes(x = index_myers, y = quadratic_distance, color = as.factor(year))) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "green") +
  labs(title = "Scatter Plot: Myers Index vs Quadratic Distance",
       x = "Myers Index", y = "Quadratic Distance", color = "Year") +
  theme_minimal()

scatter_mae <- ggplot(results, aes(x = index_myers, y = mae, color = as.factor(year))) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "orange") +
  labs(title = "Scatter Plot: Myers Index vs MAE",
       x = "Myers Index", y = "MAE", color = "Year") +
  theme_minimal()

# Guardar cada gráfico en un archivo PNG
ggsave(filename = file.path(output_path_PNG_modified, "scatter_linear_distance.png"),
       plot = scatter_linear, width = 8, height = 6, dpi = 300)

ggsave(filename = file.path(output_path_PNG_modified, "scatter_quadratic_distance.png"),
       plot = scatter_quadratic, width = 8, height = 6, dpi = 300)

ggsave(filename = file.path(output_path_PNG_modified, "scatter_mae.png"),
       plot = scatter_mae, width = 8, height = 6, dpi = 300)
# 8. Save results with an extra sheet for correlations
correlations_by_year <- correlations_long %>%
  group_by(year, metric) %>%
  summarise(mean_correlation = mean(correlation, na.rm = TRUE))

write_xlsx(list("Results" = results, "Correlations" = correlations_by_year), 
           file.path(output_path_excel_modified, "results_with_myers.xlsx"))
