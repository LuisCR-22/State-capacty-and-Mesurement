########################################
# GRD Colombia Integration
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2026-03-05
# Purpose: Integrate Global Resources Dataset (GRD) with the existing Colombia
#          state capacity and poverty analysis dataset.
#
# This script:
# 1. Loads and filters GRD to Colombia (wb_ccode == "COL")
# 2. Spatial join: assigns Colombian department (region_number) to each mine
#    using latitude/longitude coordinates (WGS84) and GADM Level 1 boundaries
# 3. Aggregates mine-level GRD data to a department x year panel
# 4. Left-joins the aggregated GRD onto the analysis dataset
#    (all master observations are preserved)
# 5. Saves the integrated dataset as
#    08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta
#
# KEY DESIGN DECISIONS:
# - Spatial join (not admin1 text): admin1 has confirmed spelling errors in the
#   GRD (e.g. "atioquia", "la guajra", "chusaca") that would cause missed matches
# - Left join: preserves all 2008-2024 master observations
# - GRD covers Colombia 1994-2014 only: years 2015-2024 will have NA in grd_*
#   variables. NA = data not available, NOT zero resources.
# - GRD has no urban/rural split: same resource values assigned to both urban=0
#   and urban=1 rows of the same dept-year (correct: natural resources are a
#   department-level geographic attribute, not urban/rural specific)

########################################
# 0. SETUP
########################################

rm(list = ls())
gc()

packages <- c("haven", "dplyr", "sf", "geodata", "tidyverse", "writexl")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

########################################
# 1. CONFIGURATION PARAMETERS
########################################

# ---- UPDATE THIS PATH TO YOUR LOCAL GRD FILE ----
# File: dfhsw_GRD_public_v1.dta
grd_path <- "C:/PATH_TO_GRD/dfhsw_GRD_public_v1.dta"
# Current location on this machine:
# grd_path <- "C:/Users/User/Dropbox/Paper state capacity and welfare outcomes/Data_raw/01_GRD_v1/dfhsw_GRD_public_v1.dta"

merge_path  <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"
gadm_cache  <- file.path(merge_path, "..", "Shapefiles")
output_path <- merge_path

dir.create(gadm_cache,  showWarnings = FALSE, recursive = TRUE)
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

########################################
# 2. DANE DEPARTMENT CODE LOOKUP TABLE
########################################
# Maps GADM NAME_1 to region_number (DANE codes for 24 survey departments).
# GADM uses ASCII names without accents/tildes.

dept_lookup <- tibble(
  NAME_1 = c(
    "Antioquia",  "Atlantico",          "Bogota D.C.",  "Bolivar",
    "Boyaca",     "Caldas",             "Caqueta",      "Cauca",
    "Cesar",      "Cordoba",            "Cundinamarca", "Choco",
    "Huila",      "La Guajira",         "Magdalena",    "Meta",
    "Narino",     "Norte de Santander", "Quindio",      "Risaralda",
    "Santander",  "Sucre",              "Tolima",       "Valle del Cauca"
  ),
  region_number = c(
     5,  8, 11, 13,
    15, 17, 18, 19,
    20, 23, 25, 27,
    41, 44, 47, 50,
    52, 54, 63, 66,
    68, 70, 73, 76
  ),
  dept_name_clean = c(
    "Antioquia",  "Atlantico",          "Bogota D.C.",  "Bolivar",
    "Boyaca",     "Caldas",             "Caqueta",      "Cauca",
    "Cesar",      "Cordoba",            "Cundinamarca", "Choco",
    "Huila",      "La Guajira",         "Magdalena",    "Meta",
    "Narino",     "Norte de Santander", "Quindio",      "Risaralda",
    "Santander",  "Sucre",              "Tolima",       "Valle del Cauca"
  )
)

########################################
# 3. LOAD AND FILTER GRD
########################################

message("Loading GRD from: ", grd_path)
grd_full <- read_dta(grd_path)
message(sprintf("Full GRD: %d obs, %d vars", nrow(grd_full), ncol(grd_full)))

grd_col <- grd_full %>% filter(wb_ccode == "COL")
message(sprintf("Colombia: %d obs  (years %d-%d)",
                nrow(grd_col), min(grd_col$year), max(grd_col$year)))
message(sprintf("  Resources: %s",
                paste(sort(unique(grd_col$resource)), collapse = ", ")))

n_miss <- sum(is.na(grd_col$latitude) | is.na(grd_col$longitude))
if (n_miss > 0) message(sprintf("WARNING: %d obs with missing coords dropped", n_miss))
grd_col_geo <- grd_col %>% filter(!is.na(latitude) & !is.na(longitude))

# Coordinate sanity check (Colombia: lat 0.67-11.76, lon -77 to -69)
n_oob <- nrow(grd_col_geo %>%
  filter(latitude < -5 | latitude > 15 | longitude < -85 | longitude > -60))
if (n_oob > 0) {
  message(sprintf("WARNING: %d obs outside Colombia bounds; nearest-feature fallback applies", n_oob))
} else {
  message("Coordinate bounds check passed.")
}

########################################
# 4. LOAD COLOMBIA DEPARTMENT SHAPEFILE (GADM)
########################################

message("\nLoading GADM Level 1 Colombia (cached at: ", gadm_cache, ")")
col_gadm <- geodata::gadm("COL", level = 1, path = gadm_cache)

col_sf <- col_gadm %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  left_join(dept_lookup, by = "NAME_1")

message(sprintf("GADM: %d territories | %d matched to region_number",
                nrow(col_sf), sum(!is.na(col_sf$region_number))))

# Print GADM names so user can update lookup table if needed
message("\nAll GADM NAME_1 values (update Section 2 if match count is unexpectedly low):")
print(sort(col_sf$NAME_1))

unmatched <- col_sf %>% filter(is.na(region_number)) %>% pull(NAME_1)
if (length(unmatched) > 0)
  message(paste("NOTE - Outside 24-dept analysis set:",
                paste(sort(unmatched), collapse = ", ")))

########################################
# 5. SPATIAL JOIN: MINE POINTS -> DEPARTMENTS
########################################

message("\nCreating spatial points from GRD coordinates (WGS84)...")
grd_sf <- grd_col_geo %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

message("Performing spatial join (st_within)...")
grd_joined <- st_join(
  grd_sf,
  col_sf[, c("NAME_1", "region_number", "dept_name_clean")],
  join = st_within
)

# Fallback: nearest department for points outside polygon boundaries
n_out <- sum(is.na(grd_joined$region_number))
if (n_out > 0) {
  message(sprintf("  %d obs outside polygons; assigning to nearest department...", n_out))
  outside <- grd_joined %>% filter(is.na(region_number))
  inside  <- grd_joined %>% filter(!is.na(region_number))
  idx     <- st_nearest_feature(outside, col_sf)
  outside$region_number   <- col_sf$region_number[idx]
  outside$dept_name_clean <- col_sf$dept_name_clean[idx]
  outside$NAME_1          <- col_sf$NAME_1[idx]
  grd_joined <- bind_rows(inside, outside)
}

message(sprintf("All %d obs assigned to a department.", nrow(grd_joined)))
message("\nDepartment assignment summary:")
print(
  grd_joined %>%
    st_drop_geometry() %>%
    count(dept_name_clean, region_number, name = "n_mines") %>%
    arrange(desc(n_mines))
)

########################################
# 6. AGGREGATE TO DEPARTMENT x YEAR
########################################

message("\nAggregating to department x year panel...")

grd_dept_year <- grd_joined %>%
  st_drop_geometry() %>%
  group_by(region_number, year) %>%
  summarise(
    grd_n_sites          = n(),
    grd_wb_value         = sum(wb_value,         na.rm = TRUE),
    grd_world_val_nomc   = sum(world_val_nomc,    na.rm = TRUE),
    grd_world_val_withmc = sum(world_val_withmc,  na.rm = TRUE),
    grd_has_resource     = 1L,
    grd_lootable_count   = as.integer(sum(lootable, na.rm = TRUE)),
    grd_has_lootable     = as.integer(sum(lootable, na.rm = TRUE) > 0),
    .groups = "drop"
  ) %>%
  mutate(
    grd_log_wb_value         = log(grd_wb_value         + 1),
    grd_log_world_val_nomc   = log(grd_world_val_nomc   + 1),
    grd_log_world_val_withmc = log(grd_world_val_withmc + 1)
  )

message(sprintf("Dept-year panel: %d obs | %d depts | years %d-%d",
                nrow(grd_dept_year),
                n_distinct(grd_dept_year$region_number),
                min(grd_dept_year$year), max(grd_dept_year$year)))

# Save intermediate file for Script 21 visualizations
inter_file <- file.path(output_path, "grd_col_dept_year_intermediate.dta")
write_dta(grd_dept_year, inter_file)
message(sprintf("Intermediate panel saved: %s", inter_file))

########################################
# 7. MERGE WITH ANALYSIS DATASET
########################################

message("\nLoading analysis dataset...")
analysis <- read_dta(file.path(merge_path, "07_V01_my_Bend_CSI_NTL_pov_depto_area.dta"))
message(sprintf("Analysis: %d obs, %d vars (years %d-%d)",
                nrow(analysis), ncol(analysis),
                min(analysis$ano, na.rm = TRUE), max(analysis$ano, na.rm = TRUE)))

# Left join: region_number + ano (= year in GRD)
# m:1 because each dept-year in GRD maps to 2 analysis rows (urban=0 and urban=1)
merged <- analysis %>%
  left_join(grd_dept_year, by = c("region_number", "ano" = "year"))

# Guard: left join must not change row count
if (nrow(merged) != nrow(analysis))
  stop(sprintf("ERROR: Row count changed! Before: %d  After: %d",
               nrow(analysis), nrow(merged)))

n_m  <- sum(!is.na(merged$grd_has_resource))
n_nm <- sum(is.na(merged$grd_has_resource))

message("\n--- MERGE RESULTS ---")
message(sprintf("Total obs:        %d", nrow(merged)))
message(sprintf("With GRD data:    %d  (%.1f%%)", n_m,  100 * n_m  / nrow(merged)))
message(sprintf("Without GRD data: %d  (%.1f%%)", n_nm, 100 * n_nm / nrow(merged)))

message("\nYears without GRD data (expected: 2015 onward):")
message(paste(
  merged %>%
    filter(is.na(grd_has_resource)) %>%
    distinct(ano) %>%
    arrange(ano) %>%
    pull(ano),
  collapse = ", "
))

message("\nDepts in analysis with no GRD mines in 2008-2014 window:")
print(
  merged %>%
    filter(ano >= 2008, ano <= 2014) %>%
    group_by(region_number) %>%
    summarise(has_grd = any(!is.na(grd_has_resource)), .groups = "drop") %>%
    filter(!has_grd) %>%
    left_join(dept_lookup %>% select(region_number, dept_name_clean), by = "region_number")
)

########################################
# 8. SAVE OUTPUT
########################################

out_file <- file.path(output_path, "08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta")
message(sprintf("\nSaving to: %s", out_file))
write_dta(merged, out_file)

message("\n=== Script 20 complete ===")
message(sprintf("Output: %s", out_file))
message("\nNew variables added:")
message("  grd_n_sites              - Count of mine/extraction sites in dept-year")
message("  grd_wb_value             - Sum of World Bank resource value (2010 USD)")
message("  grd_world_val_nomc       - Sum of WB or USGS value (excl. Multicolour)")
message("  grd_world_val_withmc     - Sum of WB, USGS, or Multicolour value")
message("  grd_has_resource         - Binary: 1 = any resource site present")
message("  grd_lootable_count       - Count of lootable resource sites")
message("  grd_has_lootable         - Binary: 1 = any lootable resource site")
message("  grd_log_wb_value         - log(grd_wb_value + 1)")
message("  grd_log_world_val_nomc   - log(grd_world_val_nomc + 1)")
message("  grd_log_world_val_withmc - log(grd_world_val_withmc + 1)")
message("\nIMPORTANT: GRD covers Colombia 1994-2014 only.")
message("  Years 2015-2024 have NA in all grd_* variables.")
message("  NA = data not available, NOT zero resources.")
message("  For regressions, restrict to 2008-2014 or add a GRD-coverage period control.")
