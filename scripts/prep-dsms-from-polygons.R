# This script reads all spatial polygons within a provided directory and creates a DTM for each one
# for upload to the drone controller. They are saved in a separate provided folder.

library(tidyverse)
library(sf)
library(elevatr)
library(terra)

# Path containing the spatial polygon KMLs (i.e. drone footprints) to create DTMs for
FOOTPRINT_PATH = "/home/derek/Downloads/hitches-01-02_blodgett-yuba-stef-inyo/Hitch_1_KMLs/Hitch_1_Plot_Boundaries/NYA/"

# Path to save the DTM outputs to
DSM_PATH = "/home/derek/Downloads/hitches-01-02_blodgett-yuba-stef-inyo/Hitch_1_DSMs/"


### Workflow

# Load all KMLs
kml_files = list.files(FOOTPRINT_PATH, pattern = "kml$", full.names = TRUE)


polys_list = map(kml_files, st_read)
polys_list = map(polys_list, st_union)
polys_list = map(polys_list, st_as_sf)
polys = bind_rows(polys_list)

# Get the filename of the KMLs so we can use the same filename for the DSMs
polys$name = basename(kml_files) |> str_remove(".kml")




# buffer more to get dem for maximum possible expansion: 1.5 km or 1500 m radius
polys_buff = polys |> st_transform(3310) |> st_buffer(1200) |> st_simplify(dTolerance = 100) |> st_transform(4326)
plot(polys_buff)


# Get the raster covering the whole area plus a 3 km buffer
polys_buff_all = polys_buff |> st_union() |> st_transform(3310) |> st_buffer(3000) |> st_transform(4326)
hull = st_convex_hull(polys_buff_all) |> st_as_sf()

dtm_all = get_elev_raster(locations = hull, src="gl1")
dtm_all = as(dtm_all, "SpatRaster")

## For every plot, crop to just the plot

for (i in 1:nrow(polys_buff)) {

  poly_buff = polys_buff[i,]
  name = poly_buff$name

  dtm_plot = crop(dtm_all, poly_buff)
  # dtm_plot = mask(dtm_plot, poly_buff)
  outfile = file.path(DSM_PATH, paste0(name, ".tif"))
  if(!dir.exists(outfile |> dirname())) {dir.create(outfile |> dirname(), recursive = TRUE)}
  writeRaster(dtm_plot, outfile, overwrite = TRUE)
  
}
