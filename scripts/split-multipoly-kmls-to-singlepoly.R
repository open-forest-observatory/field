# Take a folder of KMLs with multiple polygons and for each, save each as a single-polygon KML

library(sf)

indir = "/home/derek/Documents/repo-data-local/ofo-field_data/for-drone-crew/KML"
outdir = "/home/derek/Documents/repo-data-local/ofo-field_data/for-drone-crew/KML-singlepoly"

files = list.files(indir, recursive = TRUE, pattern = "^site-polys")


for(i in 1:length(files)) {
  
  file = files[i]
  
  d = st_read(file.path(indir,file))
  
  for(j in 1:nrow(d)) {
    
    poly = d[j,]
    
    outfile = paste0(poly$Name,".kml")
    
    outpath = file.path(outdir, outfile)
    
    st_write(poly, outpath, delete_dsn = TRUE)
    
  }

}
