########################################
# Colombia Poverty Analysis - GRD TOTAL VALUE
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2026-03-05
# Purpose: Replicate Script 19 replacing CSI index with GRD WB value.
# GRD enters as: log(wb_value+1) continuous, resource binary, lootable binary.
# NOT quintiles: limited variation; many dept-years have no mines (NA/0).
# Sample restricted to 2008-2014 (GRD coverage). No peace models (no overlap).

rm(list = ls())
gc()

packages <- c("haven", "dplyr", "broom", "sandwich", "lmtest", "writexl")
installed <- sapply(packages, require, character.only = TRUE)
if (any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta"
output_path     <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col/Poverty regressions GRD"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value <= 0.01) return("***")
  if (p_value <= 0.05) return("**")
  if (p_value <= 0.10) return("*")
  return("")
}

create_ntl_quintile_dummies <- function(data) {
  message("Creating NTL quintile dummies...")
  if ("ntl_average_masked" %in% names(data)) {
    data$ntl_quintile <- ntile(data$ntl_average_masked, 5)
    for (q in 2:5) {
      nm <- paste0("ntl_q", q)
      data[[nm]] <- ifelse(is.na(data$ntl_quintile), NA, as.integer(data$ntl_quintile == q))
    }
    dist <- table(data$ntl_quintile, useNA = "always")
    message("  NTL quintile dist: ", paste(names(dist), dist, sep="=", collapse=", "))
  }
  return(data)
}

load_and_prepare_data <- function() {
  message("Loading: ", basename(input_file_path))
  if (!file.exists(input_file_path)) stop("File not found: ", input_file_path)
  data <- haven::read_dta(input_file_path)
  message("Loaded: ", nrow(data), " obs")
  data <- data %>% filter(ano >= 2008 & ano <= 2014)
  message("After 2008-2014 filter: ", nrow(data), " obs")
  for (var in c("pov_rate_3_00", "pov_rate_4_20", "pov_rate_8_30")) {
    if (var %in% names(data)) data[[var]] <- data[[var]] / 100
  }
  for (bv in c("grd_has_resource", "grd_has_lootable")) {
    if (bv %in% names(data)) data[[bv]] <- ifelse(is.na(data[[bv]]), 0L, data[[bv]])
  }
  if ("grd_wb_value" %in% names(data)) {
    wb_fill              <- ifelse(is.na(data$grd_wb_value), 0, data$grd_wb_value)
    data$grd_log_wb_value <- log(wb_fill + 1)
    n_pos <- sum(data$grd_log_wb_value > 0, na.rm = TRUE)
    message("  grd_log_wb_value created. Positive obs: ", n_pos, "/", nrow(data))
  }
  weights_available <- "n_observations_myers" %in% names(data)
  data <- create_ntl_quintile_dummies(data)
  message("Regions:", length(unique(data$region_number)),
          "  Years:", paste(range(data$ano, na.rm=TRUE), collapse="-"))
  list(data=data, weights_available=weights_available)
}
