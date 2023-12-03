# Take a folder of KMLs with multiple polygons and for each, save each as a single-polygon KML

library(sf)
library(tidyverse)

indir = "/home/derek/Documents/repo-data-local/ofo-field_data/for-drone-crew/KML"
outdir = "/home/derek/Documents/repo-data-local/ofo-field_data/for-drone-crew/KML-singlepoly2"

files = list.files(indir, recursive = TRUE, pattern = "^site-polys")


for(i in 1:length(files)) {
  
  file = files[i]
  
  d = st_read(file.path(indir,file))
  
  for(j in 1:nrow(d)) {
    
    poly = d[j,]
    
    poly = poly |>
      mutate(Name = Name,
             Description = Name) |>
      select(Name, Description)
    
    # poly = st_cast(poly, "MULTIPOINT")
    # 
    # poly = st_zm(poly, drop = FALSE, what = "Z")
    
    outfile = paste0(poly$Name,".kml")
    
    outpath = file.path(outdir, outfile)
    
    st_write(poly, outpath, delete_dsn = TRUE)
    
  }

}
