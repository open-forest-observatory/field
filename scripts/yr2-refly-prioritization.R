# Purpose: Look at existing drone flight data and field plot locations to find the best
# opportunities to expand our coverage of environmental gradients.

library(tidyverse)
library(sf)
library(here)

## Constants
datadir = readLines(here("data-dir.txt"), n = 1)

FIELD_PLOT_ATTRIBUTES_FILE = "/home/derek/Documents/repo-data-local/ofo-r/field-reference-data/field-plot-summaries/field-plot-summary.csv"
FIELD_PLOT_FOREST_TYPE_FILE = 

## Functions
# Read in plot boundary files (one file per plot) and merge to one object
read_and_merge_plot_boundaries = function(plot_boundaries_dir) {

  ## Load plot boundaries and merge to one layer
  bound_files = list.files(plot_boundaries_dir, full.names = TRUE,  pattern = ".gpkg$")
  bounds_sf_list = lapply(bound_files, sf::st_read)

  for (i in seq_along(bounds_sf_list)) {
    # remove all cols except geometry and assign plot_id based on the filename
    bounds_sf_list[[i]] = bounds_sf_list[[i]] |>
      select() |>
      mutate(plot_id = gsub(".gpkg", "", basename(bound_files[i])))
  }

  bounds = bind_rows(bounds_sf_list)

  ## Standardize boundary plot ID formatting and compute area
  bounds = bounds |>
    mutate(plot_id = str_pad(plot_id, 4, pad = "0", side = "left"))

  bounds$area_ha_sf = (sf::st_area(bounds) |> as.numeric() / 10^4) |> round(4) # Convert to hectares

  # TODO: Simplifying plot boundaries here to reduce file size of resulting leaflet map

  return(bounds)

}






drone = st_read(file.path(datadir, "yr2-refly", "drone-polys-attributed/mission_polygons.gpkg"))

# Load all field plots and merge to one
field = read_and_merge_plot_boundaries(file.path(datadir, "yr2-refly", "field-ref-plot-bounds"))
field = st_transform(field, 3310)

st_write(field, file.path(datadir, "yr2-refly", "temp", "field_plots.gpkg"), delete_dsn = TRUE)

field_attr = read_csv(FIELD_PLOT_ATTRIBUTES_FILE)


# Compute some derived cols for categorizing the polygons
drone = drone |>
  mutate(oblique = processed_pitch > 16,
         quality_flag = prop_aligned < .5 | agl_cv > 0.1,
         mission_type = case_when(
           quality_flag ~ "poor-quality",
           between(agl_median, 60, 90) & processed_pitch > 16  ~ "low-oblique",
           between(agl_median, 105, 140) & processed_pitch < 9 ~ "high-nadir",
           between(agl_median, 60, 100) & processed_pitch > 16  ~ "marginal-low-oblique",
           between(agl_median, 95, 145) & processed_pitch < 9 ~ "marginal-high-nadir",
           TRUE ~ "other"
         ))


# For each field plot, mark whether it was completely covered by suitable oblique and nadir
# missions, and also whether it was completely covered by suitable nadir

oblique = drone |>
  filter(mission_type == "low-oblique")
nadir = drone |>
  filter(mission_type == "high-nadir")

full = st_intersection(oblique, nadir)

st_write(full, file.path(datadir, "yr2-refly", "temp", "complete_coverage.gpkg"), delete_dsn = TRUE)

full_internal = st_buffer(full, -20)
nadir_internal = st_buffer(nadir, -20)

field_full = st_covered_by(field, full_internal, sparse = FALSE)
field_full = apply(field_full, 1, any)

field_nadir = st_covered_by(field, nadir_internal, sparse = FALSE)
field_nadir = apply(field_nadir, 1, any)

field$drone_coverage_full = field_full
field$drone_coverage_nadir = field_nadir

field = field |>
  mutate(drone_coverage = case_when(
    drone_coverage_full ~ "full",
    drone_coverage_nadir ~ "nadir-only",
    TRUE ~ "none"
  ))


# attach field plot attributes
field = field |>
  left_join(field_attr, by = c("plot_id" = "plot_id"))

## Start with yuba plot prioritization

yuba_vp = field |>
  filter(name_short %in% c("VP2022", "TNC2023x")) |>
  mutate(tph = n_trees / area_ha_sf.x)
  
yuba_tnc = field |>
  filter(name_short %in% c("VP2022x", "TNC2023")) |>
  mutate(tph = n_trees / area_ha_sf.x)
  
yuba = bind_rows(yuba_vp, yuba_tnc)

ggplot(yuba, aes(x = ba_ha, y = n_gt40in, color = drone_coverage)) +
  geom_point(size = 0.5) +
  #geom_point(data = yuba_tnc, size = 1) +
  geom_text(aes(label = plot_id), size = 3) +
  facet_wrap(~forest_type) +
  theme(legend.position = "bottom")


# Of the TNC plots:

# red fir: 0088*
# CMC: (0106, 0097), 0091*, 0092, (0109 or 0096), (0111, 0097 or 0100)
# white fir: 0095
# Any of: 0105

# Of the VP plots:
# red fir: 0031
# mixed evergreen: 0012
# white fir: 0001
# CMC: (2 of: 0030, 0021, 0034, 0037), 0045, (0003, 0029)

# Any of: 0007,


# Combined into overall order:

# red fir: 0088, 0031
# CMC high-pri: (0034, 0037), 0091, (0106, 0097), (0092, 0045)
# white fir: 0001
# mixed evergreen: 0012
# CMC mid-pri: (0036, 0023), 0042
# CMC low-pri: 0092, (0109 or 0096), (0111, 0097 or 0100), 0045, (0003, 0029), 0035
# JP: 0017, 0015
# Other TNC: 0105


# From looking at GIS, the following other sites need redone imagery:
# SSI: 0052 (private land spot)

# STEF
# 73-76 (SW)
# 77-80 (E)
# 69 (NW)

#SJGEO

# Delayed mort on Caldor, Creek, Dixie


# Send Oren:
# Plot polys for Blodgett and NY
# NY priority list
# Blodgett list
# 
# STEF polys and description
