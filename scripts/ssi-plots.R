# Code to prepare Yuba candidate plot locs based on existing datasets

library(tidyverse)
library(sf)
library(here)
library(readxl)

datadir = readLines(here("data-dir.txt"), n = 1)

plot1 = data.frame(plot_id = "SS-01", lat = 39.195088, lon = -120.897757)
plot2 = data.frame(plot_id = "SS-02", lat = 39.199645, lon = -120.905726)
plots = bind_rows(plot1, plot2)

plots = st_as_sf(plots, coords = c("lon", "lat"), crs = 4326)

plots = plots |>
  mutate(Name = plot_id) |>
  select(Name, plot_id)

write_csv(plots, file.path(datadir, "for-drone-crew/plot-lists/site-list_SS_v1.csv"))
st_write(plots, file.path(datadir, "selected-sites-master/site-centers_SS_v1.gpkg"), delete_dsn = TRUE)
