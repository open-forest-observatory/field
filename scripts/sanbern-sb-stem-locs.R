library(tidyverse)
library(sf)
library(here)


datadir = readLines(here("data-dir.txt"), n = 1)

d = read_csv(file.path(datadir, "field-plot-locs/sjfdp-sb/SJFDP_Env_4ha_20201110.csv"))
  
d = st_as_sf(d, coords = c("Long", "Lat"), crs = 4326)

st_write(d, file.path(datadir, "field-plot-locs/sjfdp-sb/sb-sjfdp-quadrats.gpkg"), delete_dsn = TRUE)


## pull in the additional inferred corners
a = st_read(file.path(datadir, "field-plot-locs/sjfdp-sb/sb-addl-corners.gpkg")) |> st_as_sf()

e = bind_rows(d,a)

e$id = 1:nrow(e)
e = e |> dplyr::select(id)

hull = st_convex_hull(e |> st_union())

# buffer by the right amount, minimum acceptable drone footprint. 20 m to edge of field plots + 30 m spatial error + 100 m drone photo overlap
hull_buff = hull |> st_transform(3310) |> st_buffer(20+30+100) |> st_simplify(dTolerance = 20) |> st_transform(4326)
plot(hull_buff)


# write it

st_write(hull_buff, file.path(datadir, "field-plot-locs/sjfdp-sb/sb-sjfdp-footprint.gpkg"), delete_dsn = TRUE)
