library(neonUtilities)
library(geoNEON)
library(tidyverse)
library(sf)
library(here)


datadir = readLines(here("data-dir.txt"), n = 1)

d = loadByProduct("DP1.10098.001", "SOAP")
d_apparentindividual = d$vst_apparentindividual
d_mappingandtagging = d$vst_mappingandtagging

d_mappingandtagging = getLocTOS(d_mappingandtagging, dataProd = "vst_mappingandtagging")

d = left_join(d_mappingandtagging, d_apparentindividual, by = "individualID")
# note that there appear to be duplicate entries in apparentIndividual, and there are multiple mesurements (not duplicates) as well.

# filter to trees that are mapped and have at least diameter > 5 cm or height > 5 m
d = d |>
  filter(growthForm %in% c("multi-bole tree", "sapling", "single bole tree", "small tree"),
         !is.na(adjDecimalLatitude),
         (height > 5 & !is.na(height)) | (stemDiameter > 5 & !is.na(stemDiameter)))
  
d = st_as_sf(d, coords = c("adjDecimalLongitude", "adjDecimalLatitude"), crs = 4326)

st_write(d, file.path(datadir, "field-plot-locs/soap/woody-stem-locs_2.gpkg"), delete_dsn = TRUE)


### Map the lamping plot