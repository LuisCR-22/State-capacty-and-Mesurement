########################################
# Benford Wage Analysis - Comprehensive Version with Visualizations
########################################
# Author: Luis Castellanos Rodriguez


# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse", "ggrepel", "gridExtra")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

# 1. Define file paths for input and outputs
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/GLD database"
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Outputs/Excel"
output_path_plots <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Outputs/PNG/Myers vs. Bendford"

current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_path, paste0(current_date, "-GLD_Benford_Myers_Analysis-V2", ".xlsx"))

# Create directories if they don't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots, showWarnings = FALSE, recursive = TRUE)

# 2. Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# 3. Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      n_observations = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_correlation = numeric(),
                      benford_abs_distance = numeric(),
                      benford_chi_square = numeric(),
                      benford_mae = numeric(),
                      kuiper_statistic = numeric(),
                      kuiper_pvalue = numeric(),
                      ks_statistic = numeric(),
                      ks_pvalue = numeric(),
                      stringsAsFactors = FALSE)

# 4. Define the PROPER Traditional Myers Blended Index function
calculate_traditional_myers <- function(age_data) {
  # Ensure ages are numeric
  ages <- as.numeric(age_data)
  
  # Filter to relevant age range (15-90)
  ages <- ages[ages >= 15 & ages <= 90]
  
  # If insufficient data, return NA
  if(length(ages) < 100) {
    return(list(traditional = NA, standardized = NA))
  }
  
  # Extract terminal digits
  terminal_digits <- ages %% 10
  
  # Count original frequencies for each terminal digit
  digit_counts <- numeric(10)
  for(i in 0:9) {
    digit_counts[i+1] <- sum(terminal_digits == i)
  }
  
  # Calculate the blended count for each terminal digit
  blended_counts <- numeric(10)
  for(d in 0:9) {
    sum_count <- 0
    for(j in 0:9) {
      # This uses different starting points for each terminal digit
      # to create a "blended" count
      age_start <- 10 * j + d
      if(age_start >= 15 && age_start <= 90) {
        sum_count <- sum_count + sum(ages == age_start)
      }
    }
    blended_counts[d+1] <- sum_count
  }
  
  # Calculate percent distribution
  total_count <- sum(blended_counts)
  blended_percents <- 100 * blended_counts / total_count
  
  # Calculate absolute deviations from 10%
  deviations <- abs(blended_percents - 10)
  
  # Traditional Myers Index is half the sum of absolute deviations
  traditional_myers <- sum(deviations) / 2
  
  # Standardized Myers (0-1 scale)
  # The maximum possible value is 90 (when all ages end in a single digit)
  standardized_myers <- traditional_myers / 90
  
  return(list(traditional = traditional_myers, standardized = standardized_myers))
}

# 5. Define improved Kuiper Test function
kuiper_test <- function(obs, expected) {
  obs_cdf <- cumsum(obs) / sum(obs)
  expected_cdf <- cumsum(expected) / sum(expected)
  diff <- obs_cdf - expected_cdf
  
  V <- max(diff) - min(diff)  # Kuiper statistic
  n <- sum(obs)
  
  # Improved p-value calculation for Kuiper test
  # Using asymptotic approximation from Stephens (1970)
  lambda <- (sqrt(n) + 0.155 + 0.24/sqrt(n)) * V
  p_value <- 2 * exp(-2 * lambda^2)
  
  return(list(statistic = V, p.value = p_value))
}

# 6. Process files for ARM country
country <- "ARM"
message("Processing country: ", country)

# Get list of all ARM files
country_files <- list.files(path = input_path, 
                            pattern = paste0("^", country, ".*\\.dta$"), 
                            full.names = TRUE)

if(length(country_files) == 0) {
  stop("No files found for country ", country)
}

# Process each file separately to save memory
for(file in country_files) {
  message("  Processing file: ", file)
  
  tryCatch({
    # Load the data
    dat <- read_dta(file) %>% as.data.table()
    
    # Check for required variables
    required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age")
    if(!all(required_vars %in% names(dat))) {
      message("    Skipping file due to missing required variables.")
      next
    }
    
    # Filter for unit wage = 5 (monthly) and age between 15-90
    dat <- dat[!is.na(wage_no_compen) & unitwage == 5 & age >= 15 & age <= 90]
    
    if(nrow(dat) == 0) {
      message("    No observations remain after filtering in this file.")
      next
    }
    
    # Handle zeros and remove outliers using IQR method
    dat <- dat[wage_no_compen > 0]
    
    # Calculate IQR for wage
    q1 <- quantile(dat$wage_no_compen, 0.25)
    q3 <- quantile(dat$wage_no_compen, 0.75)
    iqr_val <- q3 - q1
    
    # Define upper bound (lower bound is already handled by wage > 0)
    upper_bound <- q3 + 3 * iqr_val
    
    # Filter out extreme outliers
    dat <- dat[wage_no_compen <= upper_bound]
    
    # Process by year
    years <- unique(dat$year)
    
    for(yr in years) {
      year_data <- dat[year == yr]
      n_obs <- nrow(year_data)
      
      if(n_obs < 100) {
        message(paste("    Skipping year", yr, "due to insufficient data (n =", n_obs, ")"))
        next
      }
      
      # Extract first digit for Benford analysis
      year_data[, first_digit := as.numeric(substr(as.character(abs(wage_no_compen)), 1, 1))]
      year_data <- year_data[first_digit %in% 1:9]
      
      # Recalculate n after filtering first digits
      n_obs <- nrow(year_data)
      if(n_obs < 100) {
        message(paste("    Skipping year", yr, "due to insufficient data after first digit extraction (n =", n_obs, ")"))
        next
      }
      
      # Calculate Myers Index
      myers_result <- calculate_traditional_myers(year_data$age)
      
      # Calculate Benford metrics
      first_digits <- year_data$first_digit
      obs_table <- table(factor(first_digits, levels = 1:9))
      obs_freq <- as.numeric(obs_table) / sum(obs_table)
      
      # Improved metrics
      benford_correlation <- cor(obs_freq, benford_expected)
      benford_abs_distance <- sum(abs(obs_freq - benford_expected))
      benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
      benford_mae <- mean(abs(obs_freq - benford_expected))
      
      # Statistical tests
      kuiper_result <- kuiper_test(obs_freq, benford_expected)
      ks_result <- ks.test(obs_freq, benford_expected)
      
      # Add to results
      results <- rbind(results,
                       data.frame(countrycode = country,
                                  year = yr,
                                  n_observations = n_obs,
                                  traditional_myers = myers_result$traditional,
                                  standardized_myers = myers_result$standardized,
                                  benford_correlation = benford_correlation,
                                  benford_abs_distance = benford_abs_distance,
                                  benford_chi_square = benford_chi_square,
                                  benford_mae = benford_mae,
                                  kuiper_statistic = kuiper_result$statistic,
                                  kuiper_pvalue = kuiper_result$p.value,
                                  ks_statistic = ks_result$statistic,
                                  ks_pvalue = ks_result$p.value,
                                  stringsAsFactors = FALSE))
    }
    
    # Clear data to free memory
    rm(dat)
    gc()
    
  }, error = function(e) {
    message("    Error processing file: ", e$message)
  })
}

# 7. Create Sheet 2: Correlations at country-year level
correlations_measures <- cor(results[, c("traditional_myers", "standardized_myers", 
                                         "benford_correlation", "benford_abs_distance", 
                                         "benford_chi_square", "benford_mae",
                                         "kuiper_statistic", "ks_statistic")], 
                              use = "pairwise.complete.obs")

# 8. Create Sheet 3: Correlations at country level (in this case, just ARM)
country_correlations <- data.frame(
  countrycode = country,
  corr_myers_abs_distance = cor(results$standardized_myers, results$benford_abs_distance, use = "pairwise.complete.obs"),
  corr_myers_chi_square = cor(results$standardized_myers, results$benford_chi_square, use = "pairwise.complete.obs"),
  corr_myers_mae = cor(results$standardized_myers, results$benford_mae, use = "pairwise.complete.obs"),
  stringsAsFactors = FALSE
)

# 9. Create Sheet 4: Correlations for 5-year periods
# First determine the min and max years
min_year <- min(results$year, na.rm = TRUE)
max_year <- max(results$year, na.rm = TRUE)

# Create periods
start_year <- floor(min_year / 5) * 5
periods <- seq(start_year, ceiling(max_year / 5) * 5, by = 5)

period_correlations <- data.frame(
  period_start = numeric(),
  period_end = numeric(),
  corr_myers_abs_distance = numeric(),
  corr_myers_chi_square = numeric(),
  corr_myers_mae = numeric(),
  n_observations = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:(length(periods) - 1)) {
  period_start <- periods[i]
  period_end <- periods[i+1] - 1
  
  period_data <- results[results$year >= period_start & results$year <= period_end, ]
  
  if(nrow(period_data) >= 3) {  # Need at least 3 observations for meaningful correlation
    period_correlations <- rbind(period_correlations,
                                 data.frame(
                                   period_start = period_start,
                                   period_end = period_end,
                                   corr_myers_abs_distance = cor(period_data$standardized_myers, 
                                                                period_data$benford_abs_distance, 
                                                                use = "pairwise.complete.obs"),
                                   corr_myers_chi_square = cor(period_data$standardized_myers, 
                                                              period_data$benford_chi_square, 
                                                              use = "pairwise.complete.obs"),
                                   corr_myers_mae = cor(period_data$standardized_myers, 
                                                       period_data$benford_mae, 
                                                       use = "pairwise.complete.obs"),
                                   n_observations = nrow(period_data),
                                   stringsAsFactors = FALSE
                                 ))
  }
}

# 10. Create Sheet 5: Explanation of columns
explanations <- data.frame(
  sheet = character(),
  column = character(),
  description = character(),
  stringsAsFactors = FALSE
)

# Add explanations for Sheet 1
sheet1_columns <- c(
  "countrycode", "Country code",
  "year", "Year of the survey",
  "n_observations", "Number of observations used in calculations",
  "traditional_myers", "Traditional Myers Blended Index (0-90 scale) - measures digit preference in age reporting",
  "standardized_myers", "Standardized Myers Index (0-1 scale) - normalized version of the Myers Index",
  "benford_correlation", "Correlation between observed first digit frequencies and Benford's Law expected frequencies",
  "benford_abs_distance", "Sum of absolute differences between observed and expected Benford frequencies",
  "benford_chi_square", "Chi-square statistic for goodness of fit to Benford's Law",
  "benford_mae", "Mean Absolute Error between observed and expected Benford frequencies",
  "kuiper_statistic", "Kuiper test statistic - measures maximum deviation in cumulative distributions",
  "kuiper_pvalue", "P-value for the Kuiper test",
  "ks_statistic", "Kolmogorov-Smirnov test statistic",
  "ks_pvalue", "P-value for the Kolmogorov-Smirnov test"
)

for(i in seq(1, length(sheet1_columns), by = 2)) {
  explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Country-Year Results",
                          column = sheet1_columns[i],
                          description = sheet1_columns[i+1],
                          stringsAsFactors = FALSE
                        ))
}

# Add explanations for Sheet 2
explanations <- rbind(explanations,
                      data.frame(
                        sheet = "Correlations Matrix",
                        column = "All columns",
                        description = "Correlation matrix between all indicators at country-year level",
                        stringsAsFactors = FALSE
                      ))

# Add explanations for Sheet 3
sheet3_columns <- c(
  "countrycode", "Country code",
  "corr_myers_abs_distance", "Correlation between standardized Myers Index and absolute distance from Benford's Law",
  "corr_myers_chi_square", "Correlation between standardized Myers Index and chi-square statistic",
  "corr_myers_mae", "Correlation between standardized Myers Index and Mean Absolute Error"
)

for(i in seq(1, length(sheet3_columns), by = 2)) {
  explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Country Correlations",
                          column = sheet3_columns[i],
                          description = sheet3_columns[i+1],
                          stringsAsFactors = FALSE
                        ))
}

# Add explanations for Sheet 4
sheet4_columns <- c(
  "period_start", "Starting year of the 5-year period",
  "period_end", "Ending year of the 5-year period",
  "corr_myers_abs_distance", "Correlation between standardized Myers Index and absolute distance from Benford's Law for this period",
  "corr_myers_chi_square", "Correlation between standardized Myers Index and chi-square statistic for this period",
  "corr_myers_mae", "Correlation between standardized Myers Index and Mean Absolute Error for this period",
  "n_observations", "Number of country-year observations in this period"
)

for(i in seq(1, length(sheet4_columns), by = 2)) {
  explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Period Correlations",
                          column = sheet4_columns[i],
                          description = sheet4_columns[i+1],
                          stringsAsFactors = FALSE
                        ))
}

# 11. NEW: Create scatter plots for country-year data
# Function to create standardized scatter plot
create_scatter_plot <- function(data, x_var = "standardized_myers", 
                                y_var, y_label, correlation_value = NULL,
                                point_labels = TRUE, weighted = FALSE) {
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    theme_minimal() +
    theme(
      # Make the entire plot background white
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "bottom",
      # Ensure the legend background is also white
      legend.background = element_rect(fill = "white", color = NA),
      # Ensure the plot margin is white and sufficient
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      x = "Standardized Myers Index (0-1)",
      y = y_label,
      title = paste0("Relationship between Myers Index and ", y_label)
    )
  
  # Add points (weighted or not)
  if (weighted) {
    p <- p + geom_point(aes(size = n_observations), alpha = 0.7)
  } else {
    p <- p + geom_point(alpha = 0.8)
  }
  
  # Add regression lines
  if (weighted) {
    # Linear regression (blue, dashed)
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                         linetype = "dashed", color = "blue", 
                         aes(weight = n_observations))
    
    # Quadratic regression (red, dashed)
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
                         linetype = "dashed", color = "red", 
                         aes(weight = n_observations))
    
    # LOWESS smoothing (green, dashed)
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, 
                         linetype = "dashed", color = "green", span = 0.75, 
                         aes(weight = n_observations))
  } else {
    # Linear regression (blue, dashed)
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                         linetype = "dashed", color = "blue")
    
    # Quadratic regression (red, dashed)
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
                         linetype = "dashed", color = "red")
    
    # LOWESS smoothing (green, dashed)
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, 
                         linetype = "dashed", color = "green", span = 0.75)
  }
  
  # Add point labels if requested
  if (point_labels) {
    p <- p + geom_text_repel(
      aes(label = paste(countrycode, year)), 
      size = 2.5, 
      box.padding = 0.35, 
      point.padding = 0.5,
      force = 2,
      max.overlaps = 40
    )
  }
  
  # Add correlation value annotation if provided
  if (!is.null(correlation_value) && !is.na(correlation_value)) {
    p <- p + annotate(
      "text", 
      x = min(data[[x_var]], na.rm = TRUE) + 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
      y = max(data[[y_var]], na.rm = TRUE) - 0.05 * diff(range(data[[y_var]], na.rm = TRUE)),
      label = paste("Correlation:", round(correlation_value, 3)),
      hjust = 0, 
      fontface = "bold"
    )
  }
  
  # Add legend for weighted plots
  if (weighted) {
    p <- p + labs(size = "Number of observations") +
      guides(size = guide_legend(title.position = "top"))
  }
  
  # Add correlation legend
  p <- p + annotate(
    "text",
    x = min(data[[x_var]], na.rm = TRUE) + 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
    y = max(data[[y_var]], na.rm = TRUE) - 0.1 * diff(range(data[[y_var]], na.rm = TRUE)),
    label = "Blue line: Linear, Red line: Quadratic, Green line: LOWESS",
    hjust = 0,
    size = 3
  )
  
  return(p)
}

# Create and save plots for country-year data if we have sufficient data
if (nrow(results) >= 3) {
  # Variables to plot
  metrics <- list(
    list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
    list(var = "benford_chi_square", label = "Chi-Square Statistic"),
    list(var = "benford_mae", label = "Mean Absolute Error"),
    list(var = "benford_correlation", label = "Correlation with Benford's Law")
  )
  
  # Create unweighted plots
  for (metric in metrics) {
    # Calculate correlation
    corr_val <- cor(results$standardized_myers, results[[metric$var]], use = "pairwise.complete.obs")
    
    # Create plot
    p_unweighted <- create_scatter_plot(
      data = results,
      y_var = metric$var,
      y_label = metric$label,
      correlation_value = corr_val,
      weighted = FALSE
    )
    
    # Save plot
    ggsave(
      filename = file.path(output_path_plots, paste0("scatter_", metric$var, ".png")),
      plot = p_unweighted,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    # Create weighted plot
    p_weighted <- create_scatter_plot(
      data = results,
      y_var = metric$var,
      y_label = paste(metric$label, "(Weighted)"),
      correlation_value = corr_val,
      weighted = TRUE
    )
    
    # Save weighted plot
    ggsave(
      filename = file.path(output_path_plots, paste0("scatter_", metric$var, "_weighted.png")),
      plot = p_weighted,
      width = 10,
      height = 7,
      dpi = 300
    )
  }
}

# 12. Create period-based scatter plots (if we have enough periods)
if (nrow(period_correlations) >= 3) {
  # Prepare data
  period_data <- data.frame()
  for (i in 1:nrow(period_correlations)) {
    period_start <- period_correlations$period_start[i]
    period_end <- period_correlations$period_end[i]
    
    # Get the data for this period
    temp_data <- results[results$year >= period_start & results$year <= period_end, ]
    
    if (nrow(temp_data) > 0) {
      # Calculate means by period
      avg_myers <- mean(temp_data$standardized_myers, na.rm = TRUE)
      avg_abs_distance <- mean(temp_data$benford_abs_distance, na.rm = TRUE)
      avg_chi_square <- mean(temp_data$benford_chi_square, na.rm = TRUE)
      avg_mae <- mean(temp_data$benford_mae, na.rm = TRUE)
      n_obs <- sum(temp_data$n_observations, na.rm = TRUE)
      
      period_data <- rbind(period_data, data.frame(
        period = paste0(period_start, "-", period_end),
        standardized_myers = avg_myers,
        benford_abs_distance = avg_abs_distance,
        benford_chi_square = avg_chi_square,
        benford_mae = avg_mae,
        n_observations = n_obs
      ))
    }
  }
  
  # Create period plots if we have data
  if (nrow(period_data) >= 3) {
    # Variables to plot
    period_metrics <- list(
      list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
      list(var = "benford_chi_square", label = "Chi-Square Statistic"),
      list(var = "benford_mae", label = "Mean Absolute Error")
    )
    
    # Create plots
    for (metric in period_metrics) {
      # Calculate correlation
      corr_val <- cor(period_data$standardized_myers, period_data[[metric$var]], use = "pairwise.complete.obs")
      
      # Create unweighted period plot
      p_period <- create_scatter_plot(
        data = period_data,
        y_var = metric$var,
        y_label = paste("Period Average", metric$label),
        correlation_value = corr_val,
        point_labels = FALSE,
        weighted = FALSE
      )
      
      # Add period labels instead of country-year
      p_period <- p_period + geom_text_repel(
        aes(label = period),
        size = 3,
        box.padding = 0.35,
        point.padding = 0.5
      )
      
      # Save period plot
      ggsave(
        filename = file.path(output_path_plots, paste0("period_scatter_", metric$var, ".png")),
        plot = p_period,
        width = 8,
        height = 6,
        dpi = 300
      )
      
      # Create weighted period plot
      p_period_weighted <- create_scatter_plot(
        data = period_data,
        y_var = metric$var,
        y_label = paste("Period Average", metric$label, "(Weighted)"),
        correlation_value = corr_val,
        point_labels = FALSE,
        weighted = TRUE
      )
      
      # Add period labels
      p_period_weighted <- p_period_weighted + geom_text_repel(
        aes(label = period),
        size = 3,
        box.padding = 0.35,
        point.padding = 0.5
      )
      
      # Save weighted period plot
      ggsave(
        filename = file.path(output_path_plots, paste0("period_scatter_", metric$var, "_weighted.png")),
        plot = p_period_weighted,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  }
}

# 13. Write all sheets to Excel
list_of_data <- list(
  "Country-Year Results" = results,
  "Correlations Matrix" = as.data.frame(correlations_measures),
  "Country Correlations" = country_correlations,
  "Period Correlations" = period_correlations,
  "Column Explanations" = explanations
)

# Export to Excel
write_xlsx(list_of_data, output_file)

message("Analysis complete. Results saved to ", output_file)
message("Scatter plots saved to ", output_path_plots)
