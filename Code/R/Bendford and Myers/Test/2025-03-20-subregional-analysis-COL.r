# ########################################
# Benford Wage Analysis - Regional Analysis Version
# ########################################
# Author: Luis Castellanos

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", 
              "tidyverse", "ggrepel", "gridExtra", "RColorBrewer", "openxlsx")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

# 1. Define file paths for input and outputs
input_file <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Sedlac"
output_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Measurement repo/State-capacty-and-Mesurement/Outputs/Col Test"
output_plots <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Measurement repo/State-capacty-and-Mesurement/Outputs/Col Test/PNG"

# Create directories if they don't exist
dir.create(output_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_plots, showWarnings = FALSE, recursive = TRUE)

# Current date for file naming
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_excel, paste0(current_date, "-Regional_Benford_Myers_Analysis.xlsx"))

# 2. Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# 3. Initialize result data frames
# For total results
total_results <- data.frame(
  income_variable = character(),
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
  stringsAsFactors = FALSE
)

# For region_est1 results
region_est1_results <- data.frame(
  income_variable = character(),
  region_est1 = character(),  # Using character type, not factor
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
  stringsAsFactors = FALSE
)

# For region_est2 results
region_est2_results <- data.frame(
  income_variable = character(),
  region_est2 = character(),  # Using character type, not factor
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
  stringsAsFactors = FALSE
)

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

# 6. Define function to calculate Benford metrics
calculate_benford_metrics <- function(data_vector) {
  # Extract first digit
  first_digits <- as.numeric(substr(as.character(abs(data_vector)), 1, 1))
  
  # Count digits 1-9 (exclude 0)
  obs_table <- table(factor(first_digits[first_digits %in% 1:9], levels = 1:9))
  
  # Calculate observed frequencies
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  # Calculate metrics
  benford_correlation <- cor(obs_freq, benford_expected)
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  benford_mae <- mean(abs(obs_freq - benford_expected))
  
  # Statistical tests
  kuiper_result <- kuiper_test(obs_freq, benford_expected)
  ks_result <- ks.test(obs_freq, benford_expected)
  
  return(list(
    correlation = benford_correlation,
    abs_distance = benford_abs_distance,
    chi_square = benford_chi_square,
    mae = benford_mae,
    kuiper_statistic = kuiper_result$statistic,
    kuiper_pvalue = kuiper_result$p.value,
    ks_statistic = ks_result$statistic,
    ks_pvalue = ks_result$p.value,
    first_digits = first_digits
  ))
}

# 7. Function to create scatter plot with region-based colors
create_scatter_plot <- function(data, x_var = "standardized_myers", 
                               y_var, y_label, title_prefix = "", 
                               region_col = "region", weighted = FALSE) {
  
  # Explicitly convert region column to character to avoid factor level issues
  data[[region_col]] <- as.character(data[[region_col]])
  
  # Get unique regions for color coding
  regions <- unique(data[[region_col]])
  n_regions <- length(regions)
  
  # Generate a color palette with enough distinct colors
  if (n_regions <= 8) {
    region_colors <- brewer.pal(max(8, n_regions), "Set1")[1:n_regions]
  } else {
    region_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_regions)
  }
  
  # Create a named vector of colors
  names(region_colors) <- regions
  
  # Calculate correlation
  corr_val <- cor(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.margin = margin(6, 6, 6, 6),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      x = "Standardized Myers Index (0-1)",
      y = y_label,
      title = paste0(title_prefix, " Relationship between Myers Index and ", y_label),
      color = gsub("_", " ", toupper(region_col))  # Format legend title
    )
  
  # Add points with colors by region
  if (weighted) {
    p <- p + geom_point(aes_string(color = region_col, size = "n_observations"), alpha = 0.8)
  } else {
    p <- p + geom_point(aes_string(color = region_col), alpha = 0.8, size = 3)
  }
  
  # Apply the color scale
  p <- p + scale_color_manual(values = region_colors, labels = regions)
  
  # Add regression lines
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                      linetype = "dashed", color = "blue")
  
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, 
                      linetype = "dashed", color = "green", span = 0.75)
  
  # Add correlation value annotation
  p <- p + annotate(
    "text", 
    x = max(data[[x_var]], na.rm = TRUE) - 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
    y = min(data[[y_var]], na.rm = TRUE) + 0.05 * diff(range(data[[y_var]], na.rm = TRUE)),
    label = paste("Correlation:", round(corr_val, 3)),
    hjust = 1,
    fontface = "bold"
  )
  
  # Add labels for points
  p <- p + geom_text_repel(
    aes_string(label = region_col),
    size = 3.5,
    box.padding = 0.35,
    point.padding = 0.5,
    max.overlaps = 20
  )
  
  # Add legend for weighted plots
  if (weighted) {
    p <- p + labs(size = "Number of observations") +
      guides(color = guide_legend(title.position = "top"),
             size = guide_legend(title.position = "top"))
  }
  
  # Add correlation line legend
  p <- p + annotate(
    "text",
    x = max(data[[x_var]], na.rm = TRUE) - 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
    y = min(data[[y_var]], na.rm = TRUE) + 0.10 * diff(range(data[[y_var]], na.rm = TRUE)),
    label = "Blue line: Linear, Green line: LOWESS",
    hjust = 1,
    size = 3
  )
  
  return(p)
}

# 8. Find all .dta files in the input folder
sedlac_files <- list.files(path = input_file, pattern = "\\.dta$", full.names = TRUE)

if(length(sedlac_files) == 0) {
  stop("No .dta files found in input folder")
}

# 9. Process the dataset
message("Found ", length(sedlac_files), " .dta files.")
message("Processing file: ", sedlac_files[1])

# Read the data
tryCatch({
  dat <- read_dta(sedlac_files[1]) %>% as.data.table()
  
  # Check for required variables
  required_vars <- c("ila", "wage", "edad", "pondera", "pais_ocaux", "ano_ocaux", "region_est1", "region_est2")
  if(!all(required_vars %in% names(dat))) {
    missing_vars <- required_vars[!required_vars %in% names(dat)]
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Convert factor variables to character to avoid factor level issues
  dat$region_est1 <- as.character(dat$region_est1)
  dat$region_est2 <- as.character(dat$region_est2)
  dat$pais_ocaux <- as.character(dat$pais_ocaux)
  
  country_code <- unique(dat$pais_ocaux)[1]
  year <- unique(dat$ano_ocaux)[1]
  
  message("Analyzing data for country ", country_code, " and year ", year)
  
  # 10. Process for each income variable (ila and wage)
  income_vars <- c("ila", "wage")
  
  for(income_var in income_vars) {
    message("Processing income variable: ", income_var)
    
    # Filter data: remove missing values, zeros, and outliers
    dat_filtered <- dat[!is.na(get(income_var)) & get(income_var) > 0 & !is.na(edad) & 
                         edad >= 15 & edad <= 90 & !is.na(pondera)]
    
    # Calculate IQR for income variable
    q1 <- quantile(dat_filtered[[income_var]], 0.25, na.rm = TRUE)
    q3 <- quantile(dat_filtered[[income_var]], 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    # Define upper bound for outliers
    upper_bound <- q3 + 3 * iqr_val
    
    # Remove outliers
    dat_filtered <- dat_filtered[get(income_var) <= upper_bound]
    
    # Apply weights
    dat_filtered[, weight_normalized := pondera / sum(pondera, na.rm = TRUE)]
    
    # 11. TOTAL ANALYSIS
    message("  Calculating metrics for total dataset")
    
    # Calculate Myers Index for age heaping
    myers_result <- calculate_traditional_myers(dat_filtered$edad)
    
    # Calculate Benford metrics
    benford_metrics <- calculate_benford_metrics(dat_filtered[[income_var]])
    
    # Add to total results
    total_results <- rbind(total_results,
                          data.frame(
                            income_variable = income_var,
                            n_observations = nrow(dat_filtered),
                            traditional_myers = myers_result$traditional,
                            standardized_myers = myers_result$standardized,
                            benford_correlation = benford_metrics$correlation,
                            benford_abs_distance = benford_metrics$abs_distance,
                            benford_chi_square = benford_metrics$chi_square,
                            benford_mae = benford_metrics$mae,
                            kuiper_statistic = benford_metrics$kuiper_statistic,
                            kuiper_pvalue = benford_metrics$kuiper_pvalue,
                            ks_statistic = benford_metrics$ks_statistic,
                            ks_pvalue = benford_metrics$ks_pvalue,
                            stringsAsFactors = FALSE
                          ))
    
    # 12. REGION_EST1 ANALYSIS
    message("  Calculating metrics by region_est1")
    
    # Get all region_est1 values
    regions_est1 <- unique(dat_filtered$region_est1)
    
    for(region in regions_est1) {
      # Filter data for this region
      region_data <- dat_filtered[region_est1 == region]
      
      # Skip if insufficient data
      if(nrow(region_data) < 100) {
        message("    Skipping region_est1 ", region, " due to insufficient data (n = ", nrow(region_data), ")")
        next
      }
      
      # Calculate Myers Index
      region_myers <- calculate_traditional_myers(region_data$edad)
      
      # Calculate Benford metrics
      region_benford <- calculate_benford_metrics(region_data[[income_var]])
      
      # Add to region_est1 results - ensure region is character
      region_est1_results <- rbind(region_est1_results,
                                 data.frame(
                                   income_variable = income_var,
                                   region_est1 = as.character(region),
                                   n_observations = nrow(region_data),
                                   traditional_myers = region_myers$traditional,
                                   standardized_myers = region_myers$standardized,
                                   benford_correlation = region_benford$correlation,
                                   benford_abs_distance = region_benford$abs_distance,
                                   benford_chi_square = region_benford$chi_square,
                                   benford_mae = region_benford$mae,
                                   kuiper_statistic = region_benford$kuiper_statistic,
                                   kuiper_pvalue = region_benford$kuiper_pvalue,
                                   ks_statistic = region_benford$ks_statistic,
                                   ks_pvalue = region_benford$ks_pvalue,
                                   stringsAsFactors = FALSE
                                 ))
    }
    
    # 13. REGION_EST2 ANALYSIS
    message("  Calculating metrics by region_est2")
    
    # Get all region_est2 values
    regions_est2 <- unique(dat_filtered$region_est2)
    
    for(region in regions_est2) {
      # Filter data for this region
      region_data <- dat_filtered[region_est2 == region]
      
      # Skip if insufficient data
      if(nrow(region_data) < 100) {
        message("    Skipping region_est2 ", region, " due to insufficient data (n = ", nrow(region_data), ")")
        next
      }
      
      # Calculate Myers Index
      region_myers <- calculate_traditional_myers(region_data$edad)
      
      # Calculate Benford metrics
      region_benford <- calculate_benford_metrics(region_data[[income_var]])
      
      # Add to region_est2 results - ensure region is character
      region_est2_results <- rbind(region_est2_results,
                                 data.frame(
                                   income_variable = income_var,
                                   region_est2 = as.character(region),
                                   n_observations = nrow(region_data),
                                   traditional_myers = region_myers$traditional,
                                   standardized_myers = region_myers$standardized,
                                   benford_correlation = region_benford$correlation,
                                   benford_abs_distance = region_benford$abs_distance,
                                   benford_chi_square = region_benford$chi_square,
                                   benford_mae = region_benford$mae,
                                   kuiper_statistic = region_benford$kuiper_statistic,
                                   kuiper_pvalue = region_benford$kuiper_pvalue,
                                   ks_statistic = region_benford$ks_statistic,
                                   ks_pvalue = region_benford$ks_pvalue,
                                   stringsAsFactors = FALSE
                                 ))
    }
  }
  
  # 14. Create summaries and correlations
  # Correlations for region_est1
  region_est1_correlations <- data.frame(
    income_variable = character(),
    corr_myers_abs_distance = numeric(),
    corr_myers_chi_square = numeric(),
    corr_myers_mae = numeric(),
    n_regions = integer(),
    stringsAsFactors = FALSE
  )
  
  for(income_var in income_vars) {
    subset_data <- region_est1_results[region_est1_results$income_variable == income_var, ]
    if(nrow(subset_data) >= 3) {
      region_est1_correlations <- rbind(region_est1_correlations,
                                     data.frame(
                                       income_variable = income_var,
                                       corr_myers_abs_distance = cor(subset_data$standardized_myers, 
                                                                    subset_data$benford_abs_distance, 
                                                                    use = "pairwise.complete.obs"),
                                       corr_myers_chi_square = cor(subset_data$standardized_myers, 
                                                                  subset_data$benford_chi_square, 
                                                                  use = "pairwise.complete.obs"),
                                       corr_myers_mae = cor(subset_data$standardized_myers, 
                                                           subset_data$benford_mae, 
                                                           use = "pairwise.complete.obs"),
                                       n_regions = nrow(subset_data),
                                       stringsAsFactors = FALSE
                                     ))
    }
  }
  
  # Correlations for region_est2
  region_est2_correlations <- data.frame(
    income_variable = character(),
    corr_myers_abs_distance = numeric(),
    corr_myers_chi_square = numeric(),
    corr_myers_mae = numeric(),
    n_regions = integer(),
    stringsAsFactors = FALSE
  )
  
  for(income_var in income_vars) {
    subset_data <- region_est2_results[region_est2_results$income_variable == income_var, ]
    if(nrow(subset_data) >= 3) {
      region_est2_correlations <- rbind(region_est2_correlations,
                                     data.frame(
                                       income_variable = income_var,
                                       corr_myers_abs_distance = cor(subset_data$standardized_myers, 
                                                                    subset_data$benford_abs_distance, 
                                                                    use = "pairwise.complete.obs"),
                                       corr_myers_chi_square = cor(subset_data$standardized_myers, 
                                                                  subset_data$benford_chi_square, 
                                                                  use = "pairwise.complete.obs"),
                                       corr_myers_mae = cor(subset_data$standardized_myers, 
                                                           subset_data$benford_mae, 
                                                           use = "pairwise.complete.obs"),
                                       n_regions = nrow(subset_data),
                                       stringsAsFactors = FALSE
                                     ))
    }
  }
  
  # 15. Create explanations sheet
  explanations <- data.frame(
    sheet = character(),
    column = character(),
    description = character(),
    stringsAsFactors = FALSE
  )
  
  # Add explanations for main results
  main_columns <- c(
    "income_variable", "Income variable analyzed (ila or wage)",
    "region_est1/region_est2", "Region identifier",
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
  
  for(i in seq(1, length(main_columns), by = 2)) {
    explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Results Tables",
                          column = main_columns[i],
                          description = main_columns[i+1],
                          stringsAsFactors = FALSE
                        ))
  }
  
  # Add explanations for correlation sheets
  corr_columns <- c(
    "income_variable", "Income variable analyzed (ila or wage)",
    "corr_myers_abs_distance", "Correlation between standardized Myers Index and absolute distance from Benford's Law",
    "corr_myers_chi_square", "Correlation between standardized Myers Index and chi-square statistic",
    "corr_myers_mae", "Correlation between standardized Myers Index and Mean Absolute Error",
    "n_regions", "Number of regions used in the correlation calculation"
  )
  
  for(i in seq(1, length(corr_columns), by = 2)) {
    explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Correlations",
                          column = corr_columns[i],
                          description = corr_columns[i+1],
                          stringsAsFactors = FALSE
                        ))
  }
  
  # 16. Create and save visualizations
  # Function to add income variable and region level to plot filename
  get_plot_filename <- function(income_var, metric, region_level, weighted = FALSE) {
    weight_suffix <- if(weighted) "_weighted" else ""
    file.path(output_plots, 
             paste0(income_var, "_", metric, "_", region_level, weight_suffix, ".png"))
  }
  
  # Define metrics to plot
  metrics <- list(
    list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
    list(var = "benford_chi_square", label = "Chi-Square Statistic"),
    list(var = "benford_mae", label = "Mean Absolute Error"),
    list(var = "benford_correlation", label = "Correlation with Benford's Law")
  )
  
  # Create plots for region_est1
  for(income_var in income_vars) {
    subset_data <- region_est1_results[region_est1_results$income_variable == income_var, ]
    
    if(nrow(subset_data) >= 3) {
      for(metric in metrics) {
        # Unweighted plot
        p <- create_scatter_plot(
          data = subset_data,
          y_var = metric$var,
          y_label = metric$label,
          title_prefix = paste0(toupper(income_var), " - Region Level 1:"),
          region_col = "region_est1",
          weighted = FALSE
        )
        
        ggsave(
          filename = get_plot_filename(income_var, metric$var, "region_est1"),
          plot = p,
          width = 10,
          height = 7,
          dpi = 300
        )
        
        # Weighted plot
        p_weighted <- create_scatter_plot(
          data = subset_data,
          y_var = metric$var,
          y_label = paste0(metric$label, " (Weighted)"),
          title_prefix = paste0(toupper(income_var), " - Region Level 1:"),
          region_col = "region_est1",
          weighted = TRUE
        )
        
        ggsave(
          filename = get_plot_filename(income_var, metric$var, "region_est1", TRUE),
          plot = p_weighted,
          width = 10,
          height = 7,
          dpi = 300
        )
      }
    }
  }
  
  # Create plots for region_est2
  for(income_var in income_vars) {
    subset_data <- region_est2_results[region_est2_results$income_variable == income_var, ]
    
    if(nrow(subset_data) >= 3) {
      for(metric in metrics) {
        # Unweighted plot
        p <- create_scatter_plot(
          data = subset_data,
          y_var = metric$var,
          y_label = metric$label,
          title_prefix = paste0(toupper(income_var), " - Region Level 2:"),
          region_col = "region_est2",
          weighted = FALSE
        )
        
        ggsave(
          filename = get_plot_filename(income_var, metric$var, "region_est2"),
          plot = p,
          width = 10,
          height = 7,
          dpi = 300
        )
        
        # Weighted plot
        p_weighted <- create_scatter_plot(
          data = subset_data,
          y_var = metric$var,
          y_label = paste0(metric$label, " (Weighted)"),
          title_prefix = paste0(toupper(income_var), " - Region Level 2:"),
          region_col = "region_est2",
          weighted = TRUE
        )
        
        ggsave(
          filename = get_plot_filename(income_var, metric$var, "region_est2", TRUE),
          plot = p_weighted,
          width = 10,
          height = 7,
          dpi = 300
        )
      }
    }
  }
  
  # 17. Write all data to Excel using openxlsx instead of writexl
  # Create a workbook
  wb <- createWorkbook()
  
  # Add sheets and write data
  addWorksheet(wb, "Total Results")
  writeData(wb, "Total Results", total_results)
  
  addWorksheet(wb, "Region Level 1 Results")
  writeData(wb, "Region Level 1 Results", region_est1_results)
  
  addWorksheet(wb, "Region Level 2 Results")
  writeData(wb, "Region Level 2 Results", region_est2_results)
  
  addWorksheet(wb, "Region Level 1 Correlations")
  writeData(wb, "Region Level 1 Correlations", region_est1_correlations)
  
  addWorksheet(wb, "Region Level 2 Correlations")
  writeData(wb, "Region Level 2 Correlations", region_est2_correlations)
  
  addWorksheet(wb, "Column Explanations")
  writeData(wb, "Column Explanations", explanations)
  
  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  message("Analysis complete. Results saved to ", output_file)
  message("Scatter plots saved to ", output_plots)
  
}, error = function(e) {
  message("Error processing file: ", e$message)
  
  # If error occurs, try to save CSV files as a backup
  tryCatch({
    message("Attempting to save results as CSV files...")
    
    csv_dir <- file.path(output_excel, "csv_results")
    dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
    
    if(exists("total_results") && nrow(total_results) > 0)
      write.csv(total_results, file.path(csv_dir, "total_results.csv"), row.names = FALSE)
    
    if(exists("region_est1_results") && nrow(region_est1_results) > 0)
      write.csv(region_est1_results, file.path(csv_dir, "region_est1_results.csv"), row.names = FALSE)
    
    if(exists("region_est2_results") && nrow(region_est2_results) > 0)
      write.csv(region_est2_results, file.path(csv_dir, "region_est2_results.csv"), row.names = FALSE)
    
    message("Results saved as CSV files in: ", csv_dir)
  }, error = function(e2) {
    message("Could not save CSV files: ", e2$message)
  })
})

# Clean up
rm(list = ls())
gc()
message("Environment cleaned up.")