library(tidyverse)
library(sf)
library(here)

## STEF published data

datadir = readLines(here("data-dir.txt"), n = 1)

d = read_csv(file.path(datadir, "field-plot-locs/st-stef/RDS-2021-0061/Data/STEF_MOCPlots_8_9_10_11_Trees_2007_2016.csv")) |>
  
d = st_as_sf(d, coords = c("X_UTM", "Y_UTM"), crs = 26910)

st_write(d, file.path(datadir, "field-plot-locs/st-stef/st-stef-stems.gpkg"), delete_dsn = TRUE)


## Then loaded Bernal stem map

## Then manually created 4 polygons outlining the contiguous stem map areas
