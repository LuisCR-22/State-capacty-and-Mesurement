########################################
# GRD Colombia Visualizations
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2026-03-05
# Purpose: Produce maps and plots showing the spatial distribution of GRD
#          resources in Colombia and their interaction with the state capacity
#          and poverty variables from the analysis dataset.
#
# Outputs (all saved to GRD visualizations folder):
#  MapA_GRD_mines.png           - Mine locations colored by resource type
#  MapB_resource_value_dept.png - Choropleth of total resource value by dept
#  PlotC_value_myers.png        - Resource value vs Myers index (state capacity)
#  PlotD_value_poverty_300.png  - Resource value vs poverty rate ($3.00/day)
#  PlotD2_value_poverty_830.png - Resource value vs poverty rate ($8.30/day)
#  PlotE_lootable_myers.png     - Myers distribution by lootable/non-lootable
#  TableF_dept_summary.xlsx     - Dept-level summary table
#
# Input: Script 20 must be run first to produce:
#  - 08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta (merged analysis+GRD)
#  - grd_col_dept_year_intermediate.dta (dept-year GRD aggregates)
#  - GRD raw file (for mine-level point map)

########################################
# 0. SETUP
########################################

rm(list = ls())
gc()

packages <- c("haven", "dplyr", "sf", "geodata", "tidyverse",
              "ggplot2", "writexl", "scales", "lmtest", "sandwich")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

########################################
# 1. CONFIGURATION PARAMETERS
########################################

# ---- UPDATE THIS PATH TO YOUR LOCAL GRD FILE ----
grd_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/GRD/GRD_public_v1.dta"
# grd_path <- "C:/Users/User/Dropbox/Paper state capacity and welfare outcomes/Data_raw/01_GRD_v1/dfhsw_GRD_public_v1.dta"

merge_path  <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"
gadm_cache  <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Shapefiles"
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col/GRD visualizations"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Department lookup (same as Script 20)
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
     5,  8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27,
    41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76
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
# 2. LOAD DATA
########################################

message("Loading data...")

# Merged analysis + GRD dataset (from Script 20)
merged <- read_dta(file.path(merge_path, "08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta"))

# Department-year GRD aggregates (intermediate from Script 20)
grd_dept_year <- read_dta(file.path(merge_path, "grd_col_dept_year_intermediate.dta"))

# GRD raw (mine-level, Colombia only) for Map A
grd_col_raw <- read_dta(grd_path) %>%
  filter(wb_ccode == "COL", !is.na(latitude), !is.na(longitude))

# Colombia shapefile (GADM)
col_gadm <- geodata::gadm("COL", level = 1, path = gadm_cache)
col_sf <- col_gadm %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  left_join(dept_lookup, by = "NAME_1")

# Dept-level summaries for maps and charts
# -- Total value over all years (for choropleth)
grd_by_dept <- grd_dept_year %>%
  group_by(region_number) %>%
  summarise(
    total_wb_value   = sum(grd_wb_value,       na.rm = TRUE),
    mean_n_sites     = mean(grd_n_sites,        na.rm = TRUE),
    total_lootable   = sum(grd_lootable_count,  na.rm = TRUE),
    has_lootable     = as.integer(sum(grd_lootable_count) > 0),
    n_years          = n(),
    .groups = "drop"
  )

# -- Average Myers and poverty for analysis depts in 2008-2014 GRD window
dept_averages <- merged %>%
  filter(ano >= 2008, ano <= 2014, !is.na(standardized_myers)) %>%
  group_by(region_number) %>%
  summarise(
    avg_myers      = mean(standardized_myers, na.rm = TRUE),
    avg_pov_300    = mean(pov_rate_3_00,      na.rm = TRUE),
    avg_pov_820    = mean(pov_rate_8_30,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(grd_by_dept, by = "region_number") %>%
  left_join(dept_lookup %>% select(region_number, dept_name_clean), by = "region_number")

# Map sf: attach GRD values to department polygons
col_sf_grd <- col_sf %>%
  left_join(grd_by_dept, by = "region_number")

########################################
# 3. TOP RESOURCES FOR COLOUR PALETTE
########################################

top_resources <- grd_col_raw %>%
  count(resource, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(resource)

grd_col_plot <- grd_col_raw %>%
  mutate(resource_grp = ifelse(resource %in% top_resources, resource, "other"))

resource_colors <- c(
  setNames(scales::hue_pal()(length(top_resources)), top_resources),
  other = "grey70"
)

########################################
# MAP A: Mine locations by resource type
########################################

message("Generating Map A: mine locations...")

mapA <- ggplot() +
  geom_sf(data = col_sf, fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_point(
    data = grd_col_plot,
    aes(x = longitude, y = latitude, colour = resource_grp),
    size = 1.8, alpha = 0.75
  ) +
  scale_colour_manual(
    values = resource_colors,
    name = "Resource",
    guide = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "Global Resources Dataset: Mine Locations in Colombia",
    subtitle = paste0("GRD coverage: ",
                      min(grd_col_raw$year), "-", max(grd_col_raw$year),
                      "  |  N = ", nrow(grd_col_raw), " site-years"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "right",
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.2)
  )

ggsave(file.path(output_path, "MapA_GRD_mines.png"),
       mapA, width = 9, height = 10, dpi = 150)
message("  MapA saved.")

########################################
# MAP B: Choropleth of total resource value by dept
########################################

message("Generating Map B: total resource value by department...")

mapB <- ggplot(col_sf_grd) +
  geom_sf(aes(fill = log(total_wb_value + 1)), colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#6baed6", "#08519c"),
    na.value = "grey90",
    name = "log(Total WB\nValue + 1)"
  ) +
  labs(
    title = "Total GRD Resource Value by Colombian Department",
    subtitle = "World Bank prices (2010 USD), aggregated 1994-2014",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(colour = "grey85", linewidth = 0.2))

ggsave(file.path(output_path, "MapB_resource_value_dept.png"),
       mapB, width = 8, height = 10, dpi = 150)
message("  MapB saved.")

########################################
# PLOT C: Resource value vs Myers index
########################################

message("Generating Plot C: resource value vs Myers index...")

# Restrict to departments with GRD data
dept_plot <- dept_averages %>% filter(!is.na(total_wb_value))

plotC <- ggplot(dept_plot,
               aes(x = log(total_wb_value + 1), y = avg_myers,
                   label = dept_name_clean)) +
  geom_point(aes(colour = has_lootable > 0), size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue", linewidth = 0.8) +
  ggrepel::geom_text_repel(size = 2.8, colour = "grey30",
                           max.overlaps = 15) +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "tomato"),
                      name = "Has lootable\nresource") +
  labs(
    title = "Resource Wealth vs State Capacity (Myers Index)",
    subtitle = "Dept averages 2008-2014 | Regression line with 95% CI",
    x = "log(Total GRD World Bank Value + 1)",
    y = "Avg. Standardized Myers Index"
  ) +
  theme_bw(base_size = 11)

# Try to load ggrepel; if not available fall back to geom_text
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  message("Note: install ggrepel for better label placement in Plot C")
  plotC <- plotC +
    geom_text(size = 2.5, vjust = -0.5, colour = "grey30")
} else {
  library(ggrepel)
}

ggsave(file.path(output_path, "PlotC_value_myers.png"),
       plotC, width = 9, height = 7, dpi = 150)
message("  PlotC saved.")

########################################
# PLOT D: Resource value vs poverty rate
########################################

message("Generating Plot D: resource value vs poverty rate...")

# Use dept-year level data for 2008-2014, split by urban/rural
dept_year_plot <- merged %>%
  filter(ano >= 2008, ano <= 2014,
         !is.na(grd_wb_value), !is.na(pov_rate_3_00)) %>%
  mutate(
    area_label = ifelse(urban_area == 1, "Urban", "Rural"),
    log_grd_value = log(grd_wb_value + 1)
  )

plotD <- ggplot(dept_year_plot,
               aes(x = log_grd_value, y = pov_rate_3_00 / 100,
                   colour = area_label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_colour_manual(values = c("Urban" = "#2171b5", "Rural" = "#74c476"),
                      name = "Area") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "GRD Resource Value vs Poverty Rate (< $3.00/day, 2021 PPP)",
    subtitle = "Dept-year obs 2008-2014 | Regression lines by area type",
    x = "log(GRD World Bank Value + 1)",
    y = "Poverty Rate"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(output_path, "PlotD_value_poverty_300.png"),
       plotD, width = 9, height = 6, dpi = 150)
message("  PlotD (pov $3.00) saved.")

########################################
# PLOT D2: Resource value vs poverty rate ($8.30/day)
########################################

message("Generating Plot D2: resource value vs poverty rate ($8.30/day)...")

dept_year_plot2 <- merged %>%
  filter(ano >= 2008, ano <= 2014,
         !is.na(grd_wb_value), !is.na(pov_rate_8_30)) %>%
  mutate(
    area_label    = ifelse(urban_area == 1, "Urban", "Rural"),
    log_grd_value = log(grd_wb_value + 1)
  )

plotD2 <- ggplot(dept_year_plot2,
               aes(x = log_grd_value, y = pov_rate_8_30 / 100,
                   colour = area_label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_colour_manual(values = c("Urban" = "#2171b5", "Rural" = "#74c476"),
                      name = "Area") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "GRD Resource Value vs Poverty Rate (< $8.30/day, 2021 PPP)",
    subtitle = "Dept-year obs 2008-2014 | Regression lines by area type",
    x = "log(GRD World Bank Value + 1)",
    y = "Poverty Rate"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(output_path, "PlotD2_value_poverty_830.png"),
       plotD2, width = 9, height = 6, dpi = 150)
message("  PlotD2 (pov $8.30) saved.")

########################################
# PLOT E: Myers distribution by lootable vs non-lootable
########################################

message("Generating Plot E: Myers distribution by lootability...")

myers_loot <- merged %>%
  filter(ano >= 2008, ano <= 2014, !is.na(grd_has_lootable),
         !is.na(standardized_myers)) %>%
  mutate(lootable_label = ifelse(grd_has_lootable == 1,
                                 "Has lootable resource",
                                 "No lootable resource"))

plotE <- ggplot(myers_loot,
               aes(x = lootable_label, y = standardized_myers,
                   fill = lootable_label)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.shape = 16,
               outlier.size = 1, fill = "white", alpha = 0.8) +
  scale_fill_manual(
    values = c("Has lootable resource" = "tomato",
               "No lootable resource"  = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Myers Index Distribution: Departments With vs Without Lootable Resources",
    subtitle = "Dept-year obs 2008-2014 | Higher Myers = worse data quality / lower state capacity",
    x = NULL,
    y = "Standardized Myers Index"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

ggsave(file.path(output_path, "PlotE_lootable_myers.png"),
       plotE, width = 7, height = 6, dpi = 150)
message("  PlotE saved.")

########################################
# TABLE F: Department-level summary
########################################

message("Generating Table F: department summary...")

# Find dominant resource per department
dominant_resource <- grd_col_raw %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  { st_join(., col_sf[, c("region_number")], join = st_within) } %>%
  st_drop_geometry() %>%
  count(region_number, resource, sort = TRUE) %>%
  group_by(region_number) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(region_number, dominant_resource = resource)

# Fix nearest-feature for any unmatched
if (any(is.na(dominant_resource$region_number))) {
  dominant_resource <- dominant_resource %>% filter(!is.na(region_number))
}

table_f <- dept_averages %>%
  left_join(dominant_resource, by = "region_number") %>%
  mutate(
    total_wb_value_m = round(total_wb_value / 1e6, 1),
    avg_myers        = round(avg_myers,    3),
    avg_pov_300      = round(avg_pov_300,  1),
    avg_pov_820      = round(avg_pov_820,  1)
  ) %>%
  select(
    region_number,
    Department       = dept_name_clean,
    Avg_Myers_2008_14 = avg_myers,
    Avg_PovRate_300  = avg_pov_300,
    Avg_PovRate_820  = avg_pov_820,
    Total_WB_Value_MUSD = total_wb_value_m,
    N_Lootable_Sites = total_lootable,
    Has_Lootable     = has_lootable,
    Dominant_Resource = dominant_resource
  ) %>%
  arrange(desc(Total_WB_Value_MUSD))

write_xlsx(table_f, file.path(output_path, "TableF_dept_summary.xlsx"))
message("  TableF saved.")

########################################
# DONE
########################################

message("\n=== Script 21 complete ===")
message(sprintf("All outputs saved to:\n  %s", output_path))
message("\nFiles created:")
message("  MapA_GRD_mines.png           - Mine locations colored by resource type")
message("  MapB_resource_value_dept.png - Choropleth: total resource value by dept")
message("  PlotC_value_myers.png        - Resource value vs Myers index scatter")
message("  PlotD_value_poverty_300.png  - Resource value vs poverty rate ($3.00/day)")
message("  PlotD2_value_poverty_830.png - Resource value vs poverty rate ($8.30/day)")
message("  PlotE_lootable_myers.png     - Myers distribution violin: lootable vs not")
message("  TableF_dept_summary.xlsx     - Dept summary table")
