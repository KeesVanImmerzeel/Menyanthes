## Code to prepare internal datasets (file "R/sysdata.rda")

fname <- system.file("extdata","B38E0108001_1.csv",package="menyanthes")
hm3 <- hm_read_dino( fname )

path <- system.file("extdata","Grondwaterstanden_Put",package="menyanthes")
hm4 <- hm_read_dino_path( path )

fname <- system.file("extdata","Dino_export_18032020.zip",package="menyanthes")
hm5 <- hm_read_dino_zip( fname )

hm_filtered_on_polygon <- hm_filter_on_poly( hm1, polygn )

obs_periods <- hm_obs_periods( hm1 )

## Create file R/sysdata.rda
usethis::use_data(
  hm3,
  hm4,
  hm5,
  hm_filtered_on_polygon,
  obs_periods,
  internal = TRUE,
  overwrite = TRUE
)
