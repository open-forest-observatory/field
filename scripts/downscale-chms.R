# Code to downscale a folder of CHMs

library(tidyverse)
library(terra)
library(here)

datadir = readLines(here("data-dir.txt"), n = 1)

## Get a list of all CHM files

chms = list.files(file.path(datadir, "for-drone-crew/DSM"), pattern = "tif$", recursive = TRUE, full.names = TRUE)

for(i in 1:length(chms)) {
  
  chm_file = chms[i]
  
  chm = rast(chm_file)
  
  chm = disagg(chm, fact = 2, method = "bilinear")
  chm = disagg(chm, fact = 2, method = "bilinear")
  
  writeRaster(chm, chm_file, overwrite = TRUE)  
}

