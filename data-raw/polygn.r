## Code to prepare external "polygn" object: polygon shape to be used in example of function hm_filter_on_poly.
fname <- "data-raw/polygn.shp"
polygn <- raster::shapefile(fname)

usethis::use_data(polygn, internal=FALSE, overwrite = TRUE)
