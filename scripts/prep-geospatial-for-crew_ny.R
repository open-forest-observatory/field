# Code to prepare Yuba candidate plot locs based on existing datasets

library(tidyverse)
library(sf)
library(here)
library(readxl)
library(elevatr)
library(raster)
library(terra)

datadir = readLines(here("data-dir.txt"), n = 1)

##### For N Yuba

# load plot *centers*
plots = st_read(file.path(datadir, "selected-sites-master/site-centers_NY_v1.gpkg")) |>
  # remove the one plot way to the south
  filter(!(simple_plot_id %in% c("NY-143", "NY-537"))) |>
  st_transform(4326)

# buffer by the right amount, minimum acceptable drone footprint. 30 m radius field plots + 30 m spatial error + 50 m drone photo overlap
plots_buff = plots |> st_transform(3310) |> st_buffer(30+30+50) |> st_simplify(dTolerance = 10) |> st_transform(4326)
plot(plots_buff)

# buffer more to get dem for maximum possible expansion: 1.5 km or 1500 m radius
plots_dembuff = plots_buff |> st_buffer(1200) |> st_simplify(dTolerance = 100) |> st_transform(4326)
plot(plots_dembuff)

plots_dembuff = plots_dembuff |>
  arrange(simple_plot_id)

plots_buff = plots_buff |>
  arrange(simple_plot_id)

plots = plots |>
  arrange(simple_plot_id)

# make a multiplot buffered kml and point kml for map pilot and Avenza
st_write(plots, file.path(datadir, "for-drone-crew/KML/site-points_NY_v1.kml"), delete_dsn = TRUE)
st_write(plots_buff, file.path(datadir, "for-drone-crew/KML/site-polys_NY_v1.kml"), delete_dsn = TRUE)
# ***---> for DJI controller, needs to be saved in SD:/DJI/KML/

# save Tier 1 and Tier 2 plots separately
st_write(plots |> filter(priority == 1), file.path(datadir, "for-mapping/site-points_NY-pri1_v1.kml"), delete_dsn = TRUE)
st_write(plots |> filter(priority == 2), file.path(datadir, "for-mapping/site-points_NY-pri2_v1.kml"), delete_dsn = TRUE)
st_write(plots |> filter(priority == 3), file.path(datadir, "for-mapping/site-points_NY-pri3_v1.kml"), delete_dsn = TRUE)

# Try saving a points KML with only the full plot name
plots_onlyfullid = plots |>
  dplyr::select(Name = plot_id)
st_write(plots_onlyfullid, file.path(datadir, "for-drone-crew/KML/site-points_NY-fullname_v1.kml"), delete_dsn = TRUE)

# Get the raster covering the whole area plus a 3 km buffer
plots_dembuff_all = plots_dembuff |> st_union() |> st_transform(3310) |> st_buffer(3000) |> st_transform(4326)
hull = st_convex_hull(plots_dembuff_all)

dtm_ellips_all = get_elev_raster(plots_dembuff_all, src="gl1e")
dtm_ellips_all = mask(dtm_ellips_all, hull |> as("Spatial"))
outdir = file.path(datadir, "for-drone-crew/DSM/NY-all_ellips/NY-all_ellips.tif")
if(!dir.exists(outdir |> dirname())) {dir.create(outdir |> dirname(), recursive = TRUE)}
writeRaster(dtm_ellips_all, outdir, overwrite = TRUE)

dtm_ortho_all = get_elev_raster(plots_dembuff_all, src="gl1")
dtm_ortho_all = mask(dtm_ortho_all, hull |> as("Spatial"))
outdir = file.path(datadir, "for-drone-crew/DSM/NY-all_ortho/NY-all_ortho.tif")
if(!dir.exists(outdir |> dirname())) {dir.create(outdir |> dirname(), recursive = TRUE)}
writeRaster(dtm_ellips_all, outdir, overwrite = TRUE)


## For every plot, crop to just the plot

for(i in 1:nrow(plots_dembuff)) {
    
  plot_dembuff = plots_dembuff[i,]
  name = plot_dembuff$Name
  
  if(st_area(plot_dembuff) == units::set_units(0, m^2)) next()
  
  dtm_ellips_plot = crop(dtm_ellips_all, plot_dembuff)
  dtm_ellips_plot = mask(dtm_ellips_plot, plot_dembuff)
  outfile = file.path(datadir, paste0("for-drone-crew/DSM/", name, "_ellips/", name, "_ellips.tif"))
  if(!dir.exists(outfile |> dirname())) {dir.create(outfile |> dirname(), recursive = TRUE)}
  writeRaster(dtm_ellips_plot, outfile, overwrite = TRUE)
  
  dtm_ortho_plot = crop(dtm_ortho_all, plot_dembuff)
  dtm_ortho_plot = mask(dtm_ortho_plot, plot_dembuff)
  outfile = file.path(datadir, paste0("for-drone-crew/DSM/", name, "_ortho/", name, "_ortho.tif"))
  if(!dir.exists(outfile |> dirname())) {dir.create(outfile |> dirname(), recursive = TRUE)}
  writeRaster(dtm_ortho_plot, outfile, overwrite = TRUE)
}
