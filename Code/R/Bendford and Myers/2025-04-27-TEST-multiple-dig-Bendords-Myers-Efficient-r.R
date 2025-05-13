########################################
# Benford Multiple-Digit Analysis with Myers Index
# --- REVISED VERSION ---
########################################
# Last modification: 2025-04-27 # Adjusted to current date provided
# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

# *** TEST MODE FLAG ***
# Set to TRUE to process only a subset of files (e.g., ARM and BOL) for testing
# Set to FALSE to process all countries specified by file search pattern
test_mode <- FALSE # <-- CHANGE THIS TO FALSE FOR FULL RUN

# Install and load required packages
# Added 'benford.analysis' package
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse",
              "ggrepel", "gridExtra", "RColorBrewer", "benford.analysis")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed], dependencies = TRUE) # Added dependencies=TRUE
  # Re-load libraries after installation
  sapply(packages, require, character.only = TRUE)
}

# 1. Define file paths for input and outputs
# --- PATHS ---
# RECOMMENDATION: Use relative paths within an RStudio Project or define base paths dynamically.
# Example using relative paths assuming the script is in the project root or an 'R' subfolder:
# input_path <- file.path("data", "GLD_database") # Example relative path
# output_base <- file.path("output") # Example relative path

# --- USER ACTION REQUIRED: Define your paths here ---
# Using placeholder absolute paths from original script - MODIFY AS NEEDED
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/GLD database" #<- MODIFY IF NEEDED
output_base <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Out of Repo/Outputs" #<- MODIFY IF NEEDED
# --- End of User Action ---

# Construct output paths using the base path and current date
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_path_excel <- file.path(output_base, "Excel")
output_path_plots_base <- file.path(output_base, "PNG", "Myers vs. Bendford")
# CORRECTED: Use current_date variable for the plot subdirectory
output_path_plots <- file.path(output_path_plots_base, current_date)
output_path_v1 <- file.path(output_path_plots, "V1")
output_path_v2 <- file.path(output_path_plots, "V2")

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_v1, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_v2, showWarnings = FALSE, recursive = TRUE)

##############################################################
# PART I: HELPER FUNCTIONS (Myers Index)
# Note: Benford functions are now handled by the 'benford.analysis' package
##############################################################

# Define the PROPER Traditional Myers Blended Index function
# (Kept from original script - seems correct)
calculate_traditional_myers <- function(age_data) {
  # Ensure ages are numeric
  ages <- as.numeric(age_data)
  
  # Filter to relevant age range (15-90)
  ages <- ages[ages >= 15 & ages <= 90]
  
  # If insufficient data, return NA
  if(length(ages) < 100) {
    # Return NA for both metrics if insufficient data
    return(list(traditional = NA_real_, standardized = NA_real_))
  }
  
  # Extract terminal digits
  terminal_digits <- ages %% 10
  
  # Count original frequencies for each terminal digit using table for efficiency
  digit_counts_table <- table(factor(terminal_digits, levels = 0:9))
  
  # Calculate the blended count for each terminal digit
  # This part uses the original loop structure which is clear, although could be vectorized
  blended_counts <- numeric(10)
  names(blended_counts) <- 0:9 # Assign names for clarity
  for(d in 0:9) {
    sum_count <- 0
    # Loop through decades (represented by j)
    for(j in 0:9) {
      # Construct the age ending in digit d for decade j
      age_target <- 10 * j + d
      # Check if the target age is within the relevant range (15-90)
      # Note: The original code included age 15-19 for d=5..9, 20+ for d=0..9 etc.
      # Let's refine the condition slightly to be more explicit
      if(age_target >= 15 && age_target <= 90) {
        # Sum counts for ages == age_target (more efficient than summing logical vector)
        sum_count <- sum_count + sum(ages == age_target, na.rm = TRUE)
      }
    }
    blended_counts[as.character(d)] <- sum_count
  }
  
  # Calculate percent distribution
  total_count <- sum(blended_counts)
  # Handle potential division by zero if total_count is 0 (though unlikely with n>=100 check)
  if (total_count == 0) {
    blended_percents <- rep(NA_real_, 10)
  } else {
    blended_percents <- 100 * blended_counts / total_count
  }
  
  
  # Calculate absolute deviations from 10%
  deviations <- abs(blended_percents - 10)
  
  # Traditional Myers Index is half the sum of absolute deviations
  traditional_myers <- sum(deviations, na.rm = TRUE) / 2 # Added na.rm
  
  # Standardized Myers (0-1 scale)
  # The maximum possible value is 90 (when all ages end in a single digit)
  standardized_myers <- traditional_myers / 90
  
  return(list(traditional = traditional_myers, standardized = standardized_myers))
}


##############################################################
# PART II: DATA PROCESSING AND ANALYSIS
##############################################################

# Initialize results list (more efficient than rbind in loop)
results_list <- list()

# Get list of ALL .dta files in the input folder
all_files <- list.files(path = input_path,
                        pattern = "\\.dta$",
                        full.names = TRUE,
                        recursive = TRUE)

if(length(all_files) == 0) {
  stop(paste("No .dta files found in input folder:", input_path))
}

# --- Test Mode Filtering ---
# Filter based on basename prefix if test_mode is TRUE
if(test_mode) {
  message("*** RUNNING IN TEST MODE ***")
  # Example: Filter for files starting with ARM or BOL
  # Adjust the pattern "^ARM|^BOL" if different test files are needed
  test_pattern <- "^ARM|^BOL"
  all_files <- all_files[grepl(test_pattern, basename(all_files))]
  if(length(all_files) == 0) {
    stop(paste("No test files matching pattern '", test_pattern, "' found in input folder:", input_path))
  }
  message("Processing subset of files (", length(all_files), ") matching pattern: ", test_pattern)
} else {
  message("Running in FULL MODE - Processing all ", length(all_files), " found .dta files.")
}


# Extract country codes from the filenames
# Assuming format: COUNTRYCODE_*.dta
# Handles cases where splitting fails
country_codes_from_files <- sapply(basename(all_files), function(x) {
  parts <- strsplit(x, "_")[[1]]
  if(length(parts) > 0) {
    return(parts[1])
  } else {
    return(NA_character_) # Return NA explicitly if pattern fails
  }
}, USE.NAMES = FALSE) # Use USE.NAMES=FALSE for cleaner output

# Get unique, non-NA country codes
countries <- unique(country_codes_from_files[!is.na(country_codes_from_files)])

message("Found data for ", length(countries), " countries: ", paste(sort(countries), collapse = ", "))

# --- Main Processing Loop ---
file_counter <- 0
results_counter <- 0

for(country in countries) {
  message("Processing country: ", country)
  
  # Get list of files specifically for this country
  # Use the extracted codes to filter the full list - more efficient than list.files again
  country_files_indices <- which(country_codes_from_files == country)
  country_files <- all_files[country_files_indices]
  
  if(length(country_files) == 0) {
    # This check might be redundant now but kept for safety
    message("  No files found for country ", country, " (this shouldn't happen based on previous logic). Skipping.")
    next
  }
  
  # Process each file for the country
  for(file in country_files) {
    file_counter <- file_counter + 1
    message("  Processing file ", file_counter, "/", length(all_files), ": ", basename(file))
    
    tryCatch({
      # Load the data using haven and convert to data.table
      dat <- haven::read_dta(file) %>% as.data.table()
      
      # Check for required variables
      required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age")
      missing_vars <- setdiff(required_vars, names(dat))
      if(length(missing_vars) > 0) {
        message("    Skipping file: Missing required variables: ", paste(missing_vars, collapse=", "))
        next
      }
      
      # --- Data Filtering ---
      # 1. Ensure wage is numeric (it should be, but good practice)
      dat[, wage_no_compen := as.numeric(wage_no_compen)]
      # 2. Filter for unit wage = 5 (monthly), age 15-90, and non-missing, positive wage
      dat_filtered <- dat[!is.na(wage_no_compen) & wage_no_compen > 0 &
                            unitwage == 5 &
                            age >= 15 & age <= 90]
      
      if(nrow(dat_filtered) == 0) {
        message("    No observations remain after initial filtering (monthly wage, age 15-90, positive wage).")
        next
      }
      
      # --- Outlier Removal (Wage) ---
      # Calculate IQR for wage on the filtered data
      q1 <- quantile(dat_filtered$wage_no_compen, 0.25, na.rm = TRUE)
      q3 <- quantile(dat_filtered$wage_no_compen, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      
      # Define upper bound (using 3*IQR as in original script)
      # Ensure IQR is non-zero to avoid issues
      if (!is.na(iqr_val) && iqr_val > 0) {
        upper_bound <- q3 + 3 * iqr_val
        dat_filtered <- dat_filtered[wage_no_compen <= upper_bound]
        message("    Applied wage outlier filter (<= Q3 + 3*IQR).")
      } else {
        message("    Skipping wage outlier filter (IQR is zero or NA).")
      }
      
      
      if(nrow(dat_filtered) == 0) {
        message("    No observations remain after wage outlier removal.")
        next
      }
      
      # --- Process by Year ---
      years <- sort(unique(dat_filtered$year))
      
      for(yr in years) {
        year_data <- dat_filtered[year == yr]
        n_obs <- nrow(year_data)
        
        # Check for sufficient observations for reliable analysis (min 100 used before)
        min_obs_threshold <- 100
        if(n_obs < min_obs_threshold) {
          message(paste("    Skipping year", yr, "for country", country, ": Insufficient data (n =", n_obs, ", required >=", min_obs_threshold, ")"))
          next
        }
        
        message(paste("      Processing Year:", yr, " Country:", country, "(n =", n_obs, ")"))
        
        # --- Calculate Myers Index ---
        myers_result <- calculate_traditional_myers(year_data$age)
        # Skip if Myers index calculation failed (e.g., due to lack of age variation)
        if (is.na(myers_result$standardized)) {
          message("        Skipping year ", yr, ": Myers Index calculation resulted in NA.")
          next
        }
        
        
        # --- Benford Analysis using 'benford.analysis' package ---
        # Using first 4 digits as per original script's intent
        num_benford_digits <- 4
        # The benford() function needs positive numbers; already filtered wage_no_compen > 0
        # Run Benford analysis
        # Added sign="positive" just to be explicit, though default
        # Added discrete=FALSE assuming wage is continuous (important for ChiSq/Mantissa)
        # Added warning=FALSE to suppress potential package warnings if desired, remove if warnings are useful
        bfd_result <- benford(year_data$wage_no_compen,
                              number.of.digits = num_benford_digits,
                              sign = "positive",
                              discrete = FALSE) # Set discrete=TRUE if wage is fundamentally discrete counts
        
        # Extract relevant statistics from the benford object
        # Note: Names match common statistical tests. Consult package docs for details.
        #       We are primarily interested in goodness-of-fit measures.
        benford_mad <- MAD(bfd_result) # Mean Absolute Deviation
        chisq_result <- chisq.benford(bfd_result) # Chi-Square test results
        
        # Add results to the list
        results_list[[length(results_list) + 1]] <- data.frame(
          countrycode = country,
          year = yr,
          n_observations = n_obs,
          traditional_myers = myers_result$traditional,
          standardized_myers = myers_result$standardized,
          # Benford metrics from package:
          benford_mad = benford_mad,
          benford_chi_square = chisq_result$statistic, # Extract statistic value
          benford_chi_square_pvalue = chisq_result$p.value, # Extract p-value
          stringsAsFactors = FALSE
        )
        results_counter <- results_counter + 1
        
      } # End year loop
      
      # Clear data objects to free memory
      rm(dat, dat_filtered, year_data, bfd_result, myers_result, chisq_result)
      gc() # Explicitly call garbage collector
      
    }, error = function(e) {
      # Log error message if processing a file fails
      message("    ERROR processing file: ", basename(file))
      message("      Error message: ", e$message)
    }) # End tryCatch
  } # End file loop for country
} # End country loop

# --- Combine results ---
# Check if any results were generated
if (length(results_list) > 0) {
  results <- dplyr::bind_rows(results_list)
  message("Processing complete. Compiled results for ", nrow(results), " country-year observations.")
  # Arrange results for better readability
  results <- results %>% arrange(countrycode, year)
} else {
  stop("Processing finished, but no results were generated. Check input data and filtering steps.")
}


##############################################################
# PART III: DATA PROCESSING AND OUTPUT CREATION
##############################################################

# --- Residualization Function ---
residualize_data <- function(data, outcome_var) {
  data <- as.data.frame(data) # Ensure it's a data.frame for lm
  
  # Check for NAs in outcome variable, as lm will remove them
  if (any(is.na(data[[outcome_var]]))) {
    message("    Note: NA values found in '", outcome_var, "'. These rows will be excluded from residualization.")
  }
  
  # Require minimum observations for reliable fixed effects
  min_obs_fe <- 10 # Set a reasonable minimum
  if (nrow(na.omit(data[, c(outcome_var, "countrycode", "year")])) < min_obs_fe) {
    message("    Insufficient non-NA observations for '", outcome_var, "' (n < ", min_obs_fe,"). Returning NA residuals.")
    return(rep(NA_real_, nrow(data)))
  }
  
  
  n_countries <- length(unique(data$countrycode))
  n_years <- length(unique(data$year))
  
  formula_str <- NULL # Initialize formula string
  
  # Determine fixed effects based on variation
  if (n_countries > 1 && n_years > 1) {
    formula_str <- paste(outcome_var, "~ factor(countrycode) + factor(year)")
    fe_type <- "country and year"
  } else if (n_countries > 1 && n_years <= 1) {
    formula_str <- paste(outcome_var, "~ factor(countrycode)")
    fe_type <- "country"
  } else if (n_countries <= 1 && n_years > 1) {
    formula_str <- paste(outcome_var, "~ factor(year)")
    fe_type <- "year"
  } else {
    # Only one country-year or insufficient variation
    message("    Insufficient variation for fixed effects for '", outcome_var, "'. Centering data (demeaning) instead.")
    # Calculate residuals as deviation from the mean (handling NAs)
    residuals_vec <- data[[outcome_var]] - mean(data[[outcome_var]], na.rm = TRUE)
    return(residuals_vec)
  }
  
  # Fit the linear model with fixed effects
  message("    Calculating residuals for '", outcome_var, "' using ", fe_type, " fixed effects.")
  model_formula <- as.formula(formula_str)
  
  # Use tryCatch for lm fitting
  model_fit <- try(lm(model_formula, data = data, na.action = na.exclude), silent = TRUE)
  
  if (inherits(model_fit, "try-error")) {
    message("      ERROR fitting lm for '", outcome_var, "'. Returning NA residuals.")
    return(rep(NA_real_, nrow(data)))
  } else {
    # Extract residuals, na.exclude ensures length matches original data
    residuals_vec <- residuals(model_fit)
    return(residuals_vec)
  }
}

# --- Process Version 1: Remove Myers Outliers ---
process_version1 <- function(results_df) {
  results_v1 <- results_df
  
  # Check if standardized_myers column exists and has enough non-NA values
  if (!"standardized_myers" %in% names(results_v1) || sum(!is.na(results_v1$standardized_myers)) < 5) {
    message("Skipping Myers outlier removal: 'standardized_myers' column missing or too few non-NA values.")
    return(results_v1)
  }
  
  # Calculate outlier threshold using IQR method (1.5 * IQR)
  q1 <- quantile(results_v1$standardized_myers, 0.25, na.rm = TRUE)
  q3 <- quantile(results_v1$standardized_myers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  if (is.na(iqr) || iqr == 0) {
    message("Skipping Myers outlier removal: IQR is NA or zero.")
    myers_threshold <- Inf # Effectively no upper limit if IQR is zero/NA
  } else {
    myers_threshold <- q3 + 1.5 * iqr
    message("Myers Index outlier threshold (upper): ", round(myers_threshold, 3))
  }
  
  
  # Apply filter: keep only non-NA observations below or at the threshold
  original_count <- nrow(results_v1)
  # Ensure we only filter non-NA values; keep NAs as they are
  results_v1 <- results_v1 %>%
    filter(is.na(standardized_myers) | standardized_myers <= myers_threshold)
  
  removed_count <- original_count - nrow(results_v1)
  
  if (removed_count > 0) {
    message(removed_count, " observations removed as Myers Index outliers (",
            round(removed_count/original_count*100, 1), "% of data)")
  } else {
    message("No Myers Index outliers removed.")
  }
  
  return(results_v1)
}

# --- Process Version 2: Remove Outliers + Residualize ---
process_version2 <- function(results_df) {
  # First remove outliers using V1 logic
  results_v2 <- process_version1(results_df)
  
  # Then residualize relevant variables
  # Adjusted list: removed benford_correlation and benford_abs_distance
  vars_to_residualize <- c("standardized_myers", "benford_mad", "benford_chi_square")
  
  message("Starting residualization process for V2...")
  for (var in vars_to_residualize) {
    # Check if variable exists before trying to residualize
    if (!var %in% names(results_v2)) {
      message("  Variable '", var, "' not found in data. Skipping residualization.")
      next
    }
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var)
    message("  Created residualized variable: ", residual_name)
  }
  message("Finished residualization.")
  return(results_v2)
}


# --- Improved Scatter Plot Function ---
# Updated to handle potentially missing correlation value
# Updated labels for Benford metrics
create_improved_scatter_plot <- function(data, x_var, y_var, y_label,
                                         title_suffix = "", weighted = FALSE) {
  
  # Check if columns exist
  if(!all(c(x_var, y_var, "countrycode", "n_observations") %in% names(data))) {
    stop("Missing required columns for plotting (x_var, y_var, countrycode, n_observations).")
  }
  
  # Remove rows with NA in x or y variable for plotting
  plot_data <- data %>% filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
  
  if (nrow(plot_data) < 3) {
    message("Skipping plot: Less than 3 non-NA data points for ", y_var, " vs ", x_var)
    return(NULL) # Return NULL if not enough data
  }
  
  # Calculate correlation (only if enough data points)
  correlation_value <- cor(plot_data[[x_var]], plot_data[[y_var]], use = "pairwise.complete.obs")
  
  
  # Get unique countries for color coding
  countries <- sort(unique(plot_data$countrycode))
  n_countries <- length(countries)
  
  # Generate a color palette
  if (n_countries <= 8) {
    country_colors <- RColorBrewer::brewer.pal(max(3, n_countries), "Set1")[1:n_countries] # Ensure min 3 for brewer.pal
  } else if (n_countries <= 12) {
    country_colors <- RColorBrewer::brewer.pal(n_countries, "Set3")
  }
  else {
    # Use a potentially repeating palette for many countries
    country_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_countries)
  }
  names(country_colors) <- countries
  
  # Create labels
  x_label <- if (grepl("residual_", x_var)) "Standardized Myers Index (Residualized)" else "Standardized Myers Index (0-1)"
  y_label_full <- if (grepl("residual_", y_var)) paste(y_label, "(Residualized)") else y_label
  
  # Create base plot using tidy evaluation
  x_var_sym <- sym(x_var)
  y_var_sym <- sym(y_var)
  
  p <- ggplot(plot_data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    theme_minimal(base_size = 11) + # Slightly larger base font
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "grey80"), # Lighter border
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
      legend.position = "right",
      legend.key.size = unit(0.6, "cm"), # Adjusted key size
      plot.margin = margin(10, 10, 10, 10), # Adjusted margins
      plot.title = element_text(hjust = 0.5, face = "bold"), # Centered bold title
      plot.subtitle = element_text(hjust = 0.5) # Centered subtitle
    ) +
    labs(
      title = paste(y_label, "vs.", x_label),
      subtitle = title_suffix,
      x = x_label,
      y = y_label_full,
      color = "Country"
    )
  
  # Add points (weighted or not)
  point_aes <- aes(color = countrycode)
  if (weighted) {
    point_aes <- aes(color = countrycode, size = n_observations)
    p <- p + geom_point(point_aes, alpha = 0.7) +
      scale_size_continuous(range = c(1, 6)) # Control size range
  } else {
    p <- p + geom_point(point_aes, alpha = 0.7, size = 2) # Fixed size for unweighted
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = country_colors)
  
  # Add horizontal red line at y=0 (useful for residuals)
  if (grepl("residual_", y_var) || min(plot_data[[y_var]], na.rm=TRUE) < 0) {
    p <- p + geom_hline(yintercept = 0, color = "red", linetype = "dashed")
  }
  
  # Add regression lines with 95% CI (only if > 1 data point)
  if (nrow(plot_data) > 1) {
    smooth_aes <- if(weighted) aes(weight = n_observations) else aes()
    
    # Linear regression (black solid)
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                         linetype = "solid", color = "black", fill = "grey60", alpha = 0.25,
                         mapping = smooth_aes)
    
    # LOWESS smoothing (dark blue dashed)
    # Use span = 0.75 by default, adjust if needed
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                         linetype = "dashed", color = "darkblue", fill = "lightblue", alpha = 0.25,
                         span = 0.75,
                         mapping = smooth_aes)
  }
  
  
  # Add correlation value annotation (if calculated)
  if (!is.null(correlation_value) && !is.na(correlation_value)) {
    # Position annotation dynamically
    x_range <- range(plot_data[[x_var]], na.rm = TRUE)
    y_range <- range(plot_data[[y_var]], na.rm = TRUE)
    p <- p + annotate(
      "text",
      x = x_range[1] + 0.05 * diff(x_range), # Position near bottom-left
      y = y_range[2] - 0.05 * diff(y_range), # Position near top-left
      label = paste("Corr:", round(correlation_value, 3)),
      hjust = 0, # Left align
      vjust = 1, # Top align
      fontface = "italic",
      size = 3.5
    )
  }
  
  # Adjust legend based on weighting and number of countries
  guide_settings <- list(color = guide_legend(order = 1, title.position = "top"))
  if (weighted) {
    guide_settings$size <- guide_legend(order = 2, title.position = "top", title="Observations")
    p <- p + guides(!!!guide_settings)
  } else {
    p <- p + guides(color = guide_legend(title.position = "top"))
  }
  
  # Add annotation for lines if they were added
  if (nrow(plot_data) > 1) {
    x_range <- range(plot_data[[x_var]], na.rm = TRUE)
    y_range <- range(plot_data[[y_var]], na.rm = TRUE)
    p <- p + annotate(
      "text",
      x = x_range[1] + 0.05 * diff(x_range), # Position near bottom-left
      y = y_range[1] + 0.05 * diff(y_range), # Position near bottom-left
      label = "Lines: LM (solid), LOWESS (dashed) w/ 95% CI",
      hjust = 0, vjust = 0, size = 3, color = "grey30"
    )
  }
  
  
  # Handle legend for many countries
  if (n_countries > 20) {
    p <- p + guides(color = guide_legend(ncol = 2)) # Use 2 columns if many countries
  } else if (n_countries > 10) {
    p <- p + guides(color = guide_legend(ncol = 1)) # Default single column is fine
  }
  
  
  return(p)
}


# --- Function to create correlation tables ---
# Updated for new Benford metrics
create_correlation_tables <- function(data, is_residualized = TRUE) {
  # Define variable names based on whether data is residualized
  if (is_residualized) {
    myers_var <- "residual_standardized_myers"
    benford_vars <- c("residual_benford_mad", "residual_benford_chi_square")
    benford_labels <- c("Benford MAD", "Benford Chi-Square")
    all_vars <- c(myers_var, benford_vars)
  } else {
    myers_var <- "standardized_myers"
    benford_vars <- c("benford_mad", "benford_chi_square")
    benford_labels <- c("Benford MAD", "Benford Chi-Square")
    all_vars <- c(myers_var, benford_vars)
  }
  
  # Check if all required variables exist in the data
  missing_vars <- setdiff(c(all_vars, "countrycode"), names(data))
  if(length(missing_vars) > 0) {
    message("Cannot create correlation tables: Missing variables: ", paste(missing_vars, collapse=", "))
    # Return empty lists or data frames to avoid errors downstream
    return(list(correlations_matrix = matrix(NA, 0, 0),
                country_correlations = data.frame(countrycode=character())))
  }
  
  
  # --- Overall Correlation Matrix ---
  # Select only the numeric columns for correlation calculation
  data_for_corr <- data[, all_vars, drop = FALSE]
  # Convert to numeric explicitly in case types are mixed
  data_for_corr <- data.frame(lapply(data_for_corr, as.numeric))
  
  correlations_matrix <- cor(data_for_corr, use = "pairwise.complete.obs")
  
  # --- Country-Specific Correlations ---
  countries <- unique(data$countrycode)
  country_corr_list <- list()
  
  for (country in countries) {
    country_data <- data[data$countrycode == country, all_vars, drop = FALSE]
    # Convert to numeric
    country_data <- data.frame(lapply(country_data, as.numeric))
    
    # Check for sufficient non-NA pairs for correlation (min 3)
    if (sum(complete.cases(country_data)) >= 3) {
      country_row <- data.frame(countrycode = country)
      for (i in 1:length(benford_vars)) {
        benford_var <- benford_vars[i]
        # Create a meaningful column name
        col_name <- paste0("corr_myers_", gsub("^(residual_)?benford_", "", benford_var))
        # Calculate correlation between myers_var and the current benford_var
        corr_value <- cor(country_data[[myers_var]], country_data[[benford_var]], use = "pairwise.complete.obs")
        country_row[[col_name]] <- corr_value
      }
      country_corr_list[[length(country_corr_list) + 1]] <- country_row
    }
  } # End country loop
  
  # Combine country correlation results
  if (length(country_corr_list) > 0) {
    country_correlations_df <- dplyr::bind_rows(country_corr_list)
  } else {
    # Create an empty data frame with expected columns if no country had enough data
    expected_cols <- c("countrycode", paste0("corr_myers_", gsub("^(residual_)?benford_", "", benford_vars)))
    country_correlations_df <- data.frame(matrix(ncol = length(expected_cols), nrow = 0))
    colnames(country_correlations_df) <- expected_cols
    country_correlations_df$countrycode <- as.character(country_correlations_df$countrycode) # Ensure type
  }
  
  
  return(list(
    correlations_matrix = correlations_matrix,
    country_correlations = country_correlations_df
  ))
}


# --- Function to create explanation tables ---
# Updated for new Benford metrics
create_explanations <- function(is_residualized = TRUE) {
  explanations_list <- list()
  
  # Helper function to add rows
  add_expl <- function(sheet, col, desc) {
    explanations_list[[length(explanations_list) + 1]] <<- data.frame(
      Sheet = sheet, Column = col, Description = desc, stringsAsFactors = FALSE
    )
  }
  
  # Explanations for base columns (present in both V1 and V2 results sheets)
  sheet_name <- "Country-Year Results"
  add_expl(sheet_name, "countrycode", "Country ISO3 Code")
  add_expl(sheet_name, "year", "Year of the survey")
  add_expl(sheet_name, "n_observations", "Number of individual observations (monthly wage earners, age 15-90, post-outlier removal) used in calculations for this country-year")
  add_expl(sheet_name, "traditional_myers", "Traditional Myers Blended Index (0-90 scale) for age reporting. Higher values indicate more age heaping.")
  add_expl(sheet_name, "standardized_myers", "Standardized Myers Index (0-1 scale), normalized version.")
  add_expl(sheet_name, "benford_mad", "Mean Absolute Deviation (MAD) between observed first 4-digit wage frequencies and Benford's Law expected frequencies. Higher values indicate larger deviations.")
  add_expl(sheet_name, "benford_chi_square", "Chi-Square statistic for goodness-of-fit test against Benford's Law (first 4 digits of wage). Higher values indicate poorer fit.")
  add_expl(sheet_name, "benford_chi_square_pvalue", "P-value for the Chi-Square test against Benford's Law. Low p-values (e.g., < 0.05) suggest significant deviation from Benford's Law.")
  
  # Add explanations for residualized columns if needed
  if (is_residualized) {
    add_expl(sheet_name, "residual_standardized_myers", "Standardized Myers Index after removing country and year fixed effects.")
    add_expl(sheet_name, "residual_benford_mad", "Benford MAD after removing country and year fixed effects.")
    add_expl(sheet_name, "residual_benford_chi_square", "Benford Chi-Square statistic after removing country and year fixed effects.")
    # Note: P-value is generally not residualized.
  }
  
  # Explanations for Correlations Matrix sheet
  sheet_name <- "Correlations Matrix"
  add_expl(sheet_name, " ", "Correlation matrix showing pairwise Pearson correlations between the key indicators at the country-year level.")
  add_expl(sheet_name, "(Row/Col Names)", "Variables included in the correlation matrix.")
  
  # Explanations for Country Correlations sheet
  sheet_name <- "Country Correlations"
  add_expl(sheet_name, "countrycode", "Country ISO3 Code.")
  myers_desc <- if(is_residualized) "residualized Standardized Myers Index" else "Standardized Myers Index"
  mad_desc <- if(is_residualized) "residualized Benford MAD" else "Benford MAD"
  chisq_desc <- if(is_residualized) "residualized Benford Chi-Square" else "Benford Chi-Square"
  add_expl(sheet_name, "corr_myers_mad", paste("Within-country correlation between", myers_desc, "and", mad_desc))
  add_expl(sheet_name, "corr_myers_chi_square", paste("Within-country correlation between", myers_desc, "and", chisq_desc))
  
  # Combine list into a data frame
  explanations_df <- dplyr::bind_rows(explanations_list)
  return(explanations_df)
}


# --- Generate Outputs ---

# Process V1 (remove outliers)
results_v1 <- process_version1(results)
# Process V2 (remove outliers + residualize)
results_v2 <- process_version2(results) # V2 calls V1 internally first

# Create Correlation Tables
cor_tables_v1 <- create_correlation_tables(results_v1, is_residualized = FALSE)
cor_tables_v2 <- create_correlation_tables(results_v2, is_residualized = TRUE)

# Create Explanation Tables
explanations_v1 <- create_explanations(is_residualized = FALSE)
explanations_v2 <- create_explanations(is_residualized = TRUE)

# --- Create Plots for V1 (Non-Residualized) ---
message("Generating V1 plots...")
if(nrow(results_v1) > 0) {
  # Define metrics to plot against Myers
  metrics_v1 <- list(
    list(var = "benford_mad", label = "Benford MAD"),
    list(var = "benford_chi_square", label = "Benford Chi-Square")
  )
  x_var_v1 <- "standardized_myers"
  
  for (metric in metrics_v1) {
    # Check if metric variable exists
    if (!metric$var %in% names(results_v1)) {
      message("  Skipping V1 plot: Variable '", metric$var, "' not found.")
      next
    }
    
    # Generate unique base filename
    plot_basename <- paste0("V1_myers-vs-", gsub("benford_", "", metric$var))
    
    # Create and save unweighted plot
    p_unweighted <- create_improved_scatter_plot(
      data = results_v1, x_var = x_var_v1, y_var = metric$var,
      y_label = metric$label, title_suffix = "(Unweighted)", weighted = FALSE
    )
    if (!is.null(p_unweighted)) {
      ggsave(filename = file.path(output_path_v1, paste0(plot_basename, ".png")),
             plot = p_unweighted, width = 10, height = 7, dpi = 300, bg = "white")
    }
    
    # Create and save weighted plot
    p_weighted <- create_improved_scatter_plot(
      data = results_v1, x_var = x_var_v1, y_var = metric$var,
      y_label = metric$label, title_suffix = "(Weighted by N Observations)", weighted = TRUE
    )
    if (!is.null(p_weighted)) {
      ggsave(filename = file.path(output_path_v1, paste0(plot_basename, "_weighted.png")),
             plot = p_weighted, width = 10, height = 7, dpi = 300, bg = "white")
    }
  }
} else {
  message("Skipping V1 plots: No data in results_v1.")
}


# --- Create Plots for V2 (Residualized) ---
message("Generating V2 plots...")
if(nrow(results_v2) > 0) {
  # Define residualized metrics to plot
  metrics_v2 <- list(
    list(var = "residual_benford_mad", label = "Benford MAD"),
    list(var = "residual_benford_chi_square", label = "Benford Chi-Square")
  )
  x_var_v2 <- "residual_standardized_myers"
  
  for (metric in metrics_v2) {
    # Check if variables exist
    if (!all(c(metric$var, x_var_v2) %in% names(results_v2))) {
      message("  Skipping V2 plot: Variable '", metric$var, "' or '", x_var_v2, "' not found.")
      next
    }
    
    # Generate unique base filename
    plot_basename <- paste0("V2_myers-vs-", gsub("(residual_)?benford_", "", metric$var), "_residualized")
    
    # Create and save unweighted plot
    p_unweighted <- create_improved_scatter_plot(
      data = results_v2, x_var = x_var_v2, y_var = metric$var,
      y_label = metric$label, title_suffix = "(Residualized, Unweighted)", weighted = FALSE
    )
    if (!is.null(p_unweighted)) {
      ggsave(filename = file.path(output_path_v2, paste0(plot_basename, ".png")),
             plot = p_unweighted, width = 10, height = 7, dpi = 300, bg = "white")
    }
    
    
    # Create and save weighted plot
    p_weighted <- create_improved_scatter_plot(
      data = results_v2, x_var = x_var_v2, y_var = metric$var,
      y_label = metric$label, title_suffix = "(Residualized, Weighted by N Observations)", weighted = TRUE
    )
    if (!is.null(p_weighted)) {
      ggsave(filename = file.path(output_path_v2, paste0(plot_basename, "_weighted.png")),
             plot = p_weighted, width = 10, height = 7, dpi = 300, bg = "white")
    }
  }
} else {
  message("Skipping V2 plots: No data in results_v2.")
}

# --- Write Excel Outputs ---
excel_filename_v1 <- file.path(output_path_excel, paste0("Benford_Myers_Results_V1_", current_date, ".xlsx"))
excel_filename_v2 <- file.path(output_path_excel, paste0("Benford_Myers_Results_V2_", current_date, ".xlsx"))

# Prepare data for V1 Excel
excel_list_v1 <- list(
  `Country-Year Results` = as.data.frame(results_v1), # writexl prefers data.frames
  `Correlations Matrix` = as.data.frame(cor_tables_v1$correlations_matrix, optional = TRUE), # Keep rownames
  `Country Correlations` = as.data.frame(cor_tables_v1$country_correlations),
  `Explanations` = as.data.frame(explanations_v1)
)
# Add rownames to correlation matrix sheet if it exists
if (nrow(cor_tables_v1$correlations_matrix) > 0) {
  excel_list_v1$`Correlations Matrix` <- cbind(Variable = rownames(cor_tables_v1$correlations_matrix),
                                               excel_list_v1$`Correlations Matrix`)
}


# Prepare data for V2 Excel
excel_list_v2 <- list(
  `Country-Year Results` = as.data.frame(results_v2),
  `Correlations Matrix` = as.data.frame(cor_tables_v2$correlations_matrix, optional = TRUE), # Keep rownames
  `Country Correlations` = as.data.frame(cor_tables_v2$country_correlations),
  `Explanations` = as.data.frame(explanations_v2)
)
# Add rownames to correlation matrix sheet if it exists
if (nrow(cor_tables_v2$correlations_matrix) > 0) {
  excel_list_v2$`Correlations Matrix` <- cbind(Variable = rownames(cor_tables_v2$correlations_matrix),
                                               excel_list_v2$`Correlations Matrix`)
}


# Write V1 Excel file
tryCatch({
  writexl::write_xlsx(excel_list_v1, path = excel_filename_v1)
  message("Successfully wrote V1 Excel output to: ", excel_filename_v1)
}, error = function(e) {
  message("ERROR writing V1 Excel file: ", e$message)
})

# Write V2 Excel file
tryCatch({
  writexl::write_xlsx(excel_list_v2, path = excel_filename_v2)
  message("Successfully wrote V2 Excel output to: ", excel_filename_v2)
}, error = function(e) {
  message("ERROR writing V2 Excel file: ", e$message)
})

message("Script finished.")