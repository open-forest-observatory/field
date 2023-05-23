# Code to take plot locs from different sources and standardize them into a common format

library(tidyverse)
library(sf)
library(here)
library(readxl)

datadir = readLines(here("data-dir.txt"), n = 1)

# AOI
aoi = st_read(file.path(datadir, "aois/north_yuba_area.kml"))

### Vibrant Planet North Yuba field plots

## Actually surveyed plots
# Plot locs
plots = st_read(file.path(datadir, "field-plot-locs/vp/field_plots.gpkg")) |> st_transform(3310)

## Potential field plots: with stratification data
candidate = st_read(file.path(datadir, "field-plot-locs/vp/gis-candidate-sites-attributed.gpkg")) |> st_transform(3310)

## Potential field plots: with tiers: pull in to candidate plots
contractor_list = read_excel(file.path(datadir, "field-plot-locs/vp/plot_list.xlsx")) |>
  select(full_plot_id, plot_tier, plot_id)
candidate = left_join(candidate, contractor_list)

# Get the plot IDs of the plots that were actually surveyed
surveyed_id = plots$Plot |> str_split(fixed("-")) |> map(3) |> unlist()

candidate[candidate$plot_id %in% surveyed_id, "has_vp_data"] = TRUE
candidate[is.na(candidate$has_vp_data), "has_vp_data"] = FALSE

st_write(candidate, file.path(datadir, "tmp/candidate_plots.gpkg"), delete_dsn = TRUE)

# Figure out which of these already have drone data from TNC pilot project
plots_w_imagery = c(17, 16, 15, 151, 147, 152, 169, 155)
candidate[candidate$plot_id %in% plots_w_imagery, "has_imagery"] = TRUE
candidate[is.na(candidate$has_imagery), "has_imagery"] = FALSE

# Determine if in N Yuba
intersects = st_intersects(candidate, aoi |> st_transform(st_crs(candidate)), sparse = FALSE)[,1]
candidate$in_n_yuba = intersects

# Exclude plots where we already have imagery
candidate = candidate |>
  filter(!has_imagery) |>
  select(-has_imagery)

# If it's not already surveyed by VP and not a focal veg type, exclude
foc_veg = c("MHC", "RFR", "SMC")
candidate = candidate |>
  filter(eveg_type %in% foc_veg | has_vp_data,
         !is.na(cat_id))

st_write(candidate, file.path(datadir, "tmp/candidate_plots_flt.gpkg"), delete_dsn = TRUE)

# Priority is:
# has VP data in N Yuba
# has VP data outside N Yuba
# tier A
# tier B

candidate = candidate |>
  mutate(priority1 = has_vp_data & in_n_yuba,
         priority2 = has_vp_data & !in_n_yuba,
         priority3 = plot_tier %in% c("A", "A*"),
         priority4 = plot_tier %in% c("B", "C"))

candidate$priority = 5
candidate[candidate$priority4, "priority"] = 4
candidate[candidate$priority3, "priority"] = 3
candidate[candidate$priority2, "priority"] = 2
candidate[candidate$priority1, "priority"] = 1

candidate = candidate |>
  filter(priority %in% c(1,2,3,4))

# get the 


# save a white fir and ppn eveg
# eveg = st_read(file.path(datadir, "field-plot-locs/vp/eveg_foc_classified_exclSteep.gpkg"))
# eveg_foc = eveg[eveg$CWHR_TYPE == "PPN",]
# st_write(eveg_foc, file.path(datadir, "field-plot-locs/vp/eveg_foc_classified_ppn.gpkg"))
# eveg_foc = eveg[eveg$CWHR_TYPE == "WFR",]
# st_write(eveg_foc, file.path(datadir, "field-plot-locs/vp/eveg_foc_classified_wfr.gpkg"))
# intersect with the project areas and exclude
# eveg_foc = eveg[eveg$CWHR_TYPE == "SMC",]
# st_write(eveg_foc, file.path(datadir, "field-plot-locs/vp/eveg_foc_classified_smc.gpkg"))


### Add the new PPN and WFR candidate plots
new_candidate = st_read(file.path(datadir, "/field-plot-locs/ny-addl-wf-pp/ny-addl-wf-pp_01.gpkg"))
# assign plot IDs, starting at 500
new_candidate$plot_id = 1:nrow(new_candidate) + 500
new_candidate$priority = 1

candidate_comb = bind_rows(candidate, new_candidate)
# we want at least one from every prefix. within a prefix, prioritize using the priority tiers.
# then try to get another of each prefix, also based on the priority tiers
# to set this up, use a col for veg type prefix, then priority tiers

candidate_comb = candidate_comb |>
  mutate(new_full_plot_id = paste(cat_id, priority, plot_id, sep = "-"))

## Get whether they are in treatment areas


# make sure not too many are in planned treatments
# make sure field sampling doesn't happen in planned treatment areas

# printout map and avenza map of the candidates