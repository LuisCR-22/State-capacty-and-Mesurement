########################################
# Enhanced Plotting Code - Version 02
########################################
# Creates improved versions of all plots with:
# - Red X-axis
# - Confidence intervals for regression lines
# - "02_" prefix in filenames
# - Uses NEW input Excel files created by individual outlier removal analysis

# Load required libraries
packages <- c("readxl", "dplyr", "ggplot2", "tidyverse", 
              "ggrepel", "gridExtra", "RColorBrewer", "broom")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path (where NEW Excel files are located)
input_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"

# Output paths for new plots - SAME as specified
output_path_plots_main <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/PNG/Myers vs. Bendford/01 Myers vs Benford individual level"
output_path_plots_checks <- file.path(output_path_plots_main, "Checks")

# Create directories if they don't exist
dir.create(output_path_plots_main, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots_checks, showWarnings = FALSE, recursive = TRUE)

########################################
# ENHANCED PLOTTING FUNCTION (SAME AS ORIGINAL)
########################################

create_enhanced_scatter_plot <- function(data, x_var, y_var, y_label, version_num, 
                                         weighted = FALSE, for_checks = FALSE) {
  
  # Get unique countries for color coding
  countries <- unique(data$countrycode)
  n_countries <- length(countries)
  
  # Generate color palette
  if (n_countries <= 8) {
    country_colors <- brewer.pal(max(8, n_countries), "Set1")[1:n_countries]
  } else {
    country_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_countries)
  }
  names(country_colors) <- countries
  
  # Create labels
  if (grepl("residual_", x_var)) {
    x_label <- "Standardized Myers Index (Residualized)"
  } else {
    x_label <- "Standardized Myers Index (0-1)"
  }
  
  if (grepl("residual_", y_var)) {
    y_label <- paste(y_label, "(Residualized)")
  }
  
  # Create plot
  x_var_sym <- sym(x_var)
  y_var_sym <- sym(y_var)
  
  p <- ggplot(data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(x = x_label, y = y_label, color = "Country")
  
  # Add points
  if (weighted && "n_observations_benford" %in% names(data)) {
    p <- p + geom_point(aes(color = countrycode, size = n_observations_benford), alpha = 0.8)
    p <- p + labs(size = "N observations")
  } else {
    p <- p + geom_point(aes(color = countrycode), alpha = 0.8)
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = country_colors)
  
  # Extend y-axis limits to accommodate bottom annotations
  y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
  y_min_extended <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.2
  p <- p + coord_cartesian(ylim = c(y_min_extended, max(data[[y_var]], na.rm = TRUE) + y_range * 0.05))
  
  # Enhanced: Add regression lines WITH confidence intervals and improved colors
  # Linear regression: black solid line with gray confidence intervals
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                       linetype = "solid", color = "black", fill = "gray70", alpha = 0.4)
  # Loess regression: darker green dashed line with more transparent green intervals
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                       linetype = "dashed", color = "darkgreen", fill = "darkgreen", alpha = 0.10, span = 0.75)
  
  # Add red horizontal reference line at y=0
  p <- p + geom_hline(yintercept = 0, color = "darkred", linetype = "solid", linewidth = 0.5)
  
  # Add correlation and significance test with improved annotation style
  # Perform correlation test
  cor_test <- cor.test(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  corr_val <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create significance annotation matching the example image style
  if (!is.na(corr_val) && !is.na(p_value)) {
    # Add significance stars
    stars <- ""
    if (p_value <= 0.01) stars <- "***"
    else if (p_value <= 0.05) stars <- "**"
    else if (p_value <= 0.10) stars <- "*"
    
    # Position annotations at bottom like in the example image
    x_range <- max(data[[x_var]], na.rm = TRUE) - min(data[[x_var]], na.rm = TRUE)
    y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
    x_center <- (max(data[[x_var]], na.rm = TRUE) + min(data[[x_var]], na.rm = TRUE)) / 2
    y_bottom <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.15
    
    # Add concise line description
    p <- p + annotate("text", 
                      x = x_center,
                      y = y_bottom,
                      label = "Black line: Linear regression. Green line: LOWESS smoothing.",
                      hjust = 0.5, fontface = "plain", size = 3.2)
    
    # Add correlation info
    correlation_text <- paste0("Correlation: ", round(corr_val, 3), stars)
    p <- p + annotate("text", 
                      x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                      y = y_bottom + y_range * 0.05,
                      label = correlation_text,
                      hjust = 1, fontface = "bold", size = 3.5)
  }
  
  return(p)
}

########################################
# MAIN PROCESSING LOOP FOR NEW INPUT FILES
########################################

# Define the new files and their corresponding information
new_files_info <- list(
  "V01" = list(
    filename = "04_Myers_Benford_Ind_V01_NoOutliers.xlsx",
    sheet = "V1_Results_Data",
    description = "No outlier removal"
  ),
  "V02" = list(
    filename = "04_Myers_Benford_Ind_V02_IndividualOutliers.xlsx", 
    sheet = "V2_Results_Data",
    description = "Individual-level outlier removal"
  )
)

# Process each new file
for (version_name in names(new_files_info)) {
  file_info <- new_files_info[[version_name]]
  message("Processing ", version_name, " (", file_info$description, ") for enhanced plots...")
  
  # Read Excel file
  excel_path <- file.path(input_path_excel, file_info$filename)
  
  if (!file.exists(excel_path)) {
    message("  Excel file not found: ", file_info$filename)
    next
  }
  
  # Read data
  tryCatch({
    version_data <- read_excel(excel_path, sheet = file_info$sheet)
    
    # Extract version number for processing
    version_num <- as.numeric(substr(version_name, 3, 3))
    
    # For these new files, neither version uses residualized variables
    # They use the standard variables directly
    x_var <- "standardized_myers"
    y_vars <- list(
      "mae" = list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
      "chi_square" = list(var = "benford_chi_square", label = "Chi-Square Statistic")
    )
    
    # Create enhanced plots for each measure
    for (measure_name in names(y_vars)) {
      y_info <- y_vars[[measure_name]]
      
      # Check if we have the required variables
      if (!x_var %in% names(version_data) || !y_info$var %in% names(version_data)) {
        message("  Required variables not found for ", measure_name, " in ", version_name)
        message("  Available columns: ", paste(names(version_data), collapse = ", "))
        next
      }
      
      # Skip if insufficient data
      if (sum(complete.cases(version_data[, c(x_var, y_info$var)])) < 5) {
        message("  Insufficient data for ", measure_name, " plots in ", version_name)
        next
      }
      
      # Create enhanced unweighted plot
      p_unweighted_enhanced <- create_enhanced_scatter_plot(
        data = version_data,
        x_var = x_var,
        y_var = y_info$var,
        y_label = y_info$label,
        version_num = version_num,
        weighted = FALSE
      )
      
      # Create enhanced weighted plot (if sample size data available)
      if ("n_observations_benford" %in% names(version_data)) {
        p_weighted_enhanced <- create_enhanced_scatter_plot(
          data = version_data,
          x_var = x_var,
          y_var = y_info$var,
          y_label = y_info$label,
          version_num = version_num,
          weighted = TRUE
        )
      } else {
        p_weighted_enhanced <- p_unweighted_enhanced  # Fallback if no weight data
      }
      
      # Save enhanced plots with "02_" prefix and "_2" suffix
      unweighted_filename <- paste0("02_Myers_Benford_Ind_", version_name, "_unweighted_", measure_name, "_2.png")
      weighted_filename <- paste0("02_Myers_Benford_Ind_", version_name, "_weighted_", measure_name, "_2.png")
      
      if (measure_name == "mae") {
        # Save MAE plots in main folder
        ggsave(file.path(output_path_plots_main, unweighted_filename), 
               p_unweighted_enhanced, width = 10, height = 7, dpi = 300)
        ggsave(file.path(output_path_plots_main, weighted_filename), 
               p_weighted_enhanced, width = 10, height = 7, dpi = 300)
        message("  Saved enhanced MAE plots for ", version_name)
      } else {
        # Save other measures in checks folder
        ggsave(file.path(output_path_plots_checks, unweighted_filename), 
               p_unweighted_enhanced, width = 10, height = 7, dpi = 300)
        ggsave(file.path(output_path_plots_checks, weighted_filename), 
               p_weighted_enhanced, width = 10, height = 7, dpi = 300)
        message("  Saved enhanced ", measure_name, " plots for ", version_name)
      }
    }
    
  }, error = function(e) {
    message("  Error processing ", version_name, ": ", e$message)
  })
}

message("Enhanced plotting completed for NEW input files! All plots now have:")
message("- '02_' prefix instead of '01_'")
message("- Same visual format as original plots")
message("- Black solid regression line with gray confidence intervals")
message("- Dark green dashed LOWESS line with transparent green confidence intervals") 
message("- Red horizontal reference line at y=0")
message("- Short note explaining black and green lines")
message("- '_2' suffix in filenames")
message("Input files processed:")
message("- 04_Myers_Benford_Ind_V01_NoOutliers.xlsx (V1_Results_Data sheet)")
message("- 04_Myers_Benford_Ind_V02_IndividualOutliers.xlsx (V2_Results_Data sheet)")
message("Output plots saved with structure: 02_Myers_Benford_Ind_V0X_[weighted/unweighted]_[measure]_2.png")