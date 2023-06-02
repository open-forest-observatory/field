# Code to prepare Spasojevic candidate plot locs based on existing datasets

library(tidyverse)
library(sf)
library(here)
library(readxl)
library(elevatr)
library(raster)
library(terra)

datadir = readLines(here("data-dir.txt"), n = 1)

##### 

# load convex hulls
plots = st_read(file.path(datadir, "field-plot-locs/sjfdp-sb/sb-sjfdp-footprint.gpkg")) |>
  mutate(plot_id = paste0("SB-01")) |>
  dplyr::select(Name = plot_id)

# buffer more to get dem for maximum possible expansion: 1.5 km or 1500 m radius
plots_dembuff = plots |> st_buffer(1200) |> st_simplify(dTolerance = 100) |> st_transform(4326)
plot(plots_dembuff)

plots_dembuff = plots_dembuff |>
  arrange(Name)

plots = plots |>
  arrange(Name)

# make a multiplot buffered kml and point kml for map pilot and Avenza
st_write(st_centroid(plots), file.path(datadir, "for-drone-crew/KML/site-points_SB_v1.kml"), delete_dsn = TRUE)
st_write(plots, file.path(datadir, "for-drone-crew/KML/site-polys_SB_v1.kml"), delete_dsn = TRUE)
# ***---> for DJI controller, needs to be saved in SD:/DJI/KML/


# Get the raster covering the whole area plus a 3 km buffer
plots_dembuff_all = plots_dembuff |> st_union() |> st_transform(3310) |> st_buffer(3000) |> st_transform(4326)
hull = st_convex_hull(plots_dembuff_all)

dtm_ellips_all = get_elev_raster(plots_dembuff_all, src="gl1e")
dtm_ellips_all = mask(dtm_ellips_all, hull |> as("Spatial"))
outdir = file.path(datadir, "for-drone-crew/DSM/SB-all_ellips/SB-all_ellips.tif")
if(!dir.exists(outdir |> dirname())) {dir.create(outdir |> dirname(), recursive = TRUE)}
writeRaster(dtm_ellips_all, outdir, overwrite = TRUE)

# dtm_ortho_all = get_elev_raster(plots_dembuff_all, src="gl1")
# dtm_ortho_all = mask(dtm_ortho_all, hull |> as("Spatial"))
# outdir = file.path(datadir, "for-drone-crew/DSM/SS-all_ortho/SS-all_ortho.tif")
# if(!dir.exists(outdir |> dirname())) {dir.create(outdir |> dirname(), recursive = TRUE)}
# writeRaster(dtm_ellips_all, outdir, overwrite = TRUE)


## For every plot, crop to just the plot

for(i in 1:nrow(plots_dembuff)) {
    
  plot_dembuff = plots_dembuff[i,]
  name = plot_dembuff$Name
  
  dtm_ellips_plot = crop(dtm_ellips_all, plot_dembuff)
  dtm_ellips_plot = mask(dtm_ellips_plot, plot_dembuff)
  outfile = file.path(datadir, paste0("for-drone-crew/DSM/", name, "_ellips/", name, "_ellips.tif"))
  if(!dir.exists(outfile |> dirname())) {dir.create(outfile |> dirname(), recursive = TRUE)}
  writeRaster(dtm_ellips_plot, outfile, overwrite = TRUE)
  
  # dtm_ortho_plot = crop(dtm_ortho_all, plot_dembuff)
  # dtm_ortho_plot = mask(dtm_ortho_plot, plot_dembuff)
  # outfile = file.path(datadir, paste0("for-drone-crew/DSM/", name, "_ortho/", name, "_ortho.tif"))
  # if(!dir.exists(outfile |> dirname())) {dir.create(outfile |> dirname(), recursive = TRUE)}
  # writeRaster(dtm_ortho_plot, outfile, overwrite = TRUE)
}
