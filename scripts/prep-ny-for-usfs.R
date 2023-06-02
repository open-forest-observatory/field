# Code to thin out the candidate North Yuba plots to Tiers 1-3.

library(tidyverse)
library(sf)
library(here)
library(readxl)
library(elevatr)
library(raster)
library(terra)

datadir = readLines(here("data-dir.txt"), n = 1)

plots = st_read(file.path(datadir, "selected-sites-master/site-centers_NY_v1.gpkg")) |>
  # remove the one plot way to the south
  filter(!(simple_plot_id %in% c("NY-143", "NY-537"))) |>
  st_transform(4326) |>
  filter(priority < 4)

st_write(plots, file.path(datadir, "selected-sites-forcollabs/young-ucdavis-candidate-plots.shp"))
