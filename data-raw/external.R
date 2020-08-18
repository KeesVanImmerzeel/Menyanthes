## Code to prepare external datasets (visible to user)

fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
hm1 <- hm_read_export_csv( fname )

fname <- system.file("extdata","Topsoil1.csv",package="menyanthes")
hm2 <- hm_read_export_csv2( fname )

## Code to prepare external "polygn" object: polygon shape to be used in example of function hm_filter_on_poly.
fname <- "data-raw/polygn.shp"
polygn <- raster::shapefile(fname)

usethis::use_data(hm1, hm2, polygn, internal=FALSE, overwrite = TRUE)
