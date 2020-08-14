## Code to prepare external "hm1" example dataset (HydroMonitor Observation Well data).

fname <- system.file("extdata","Topsoil1.csv",package="menyanthes")
hm2 <- hm_read_export_csv2( fname )

usethis::use_data(hm2, internal=FALSE, overwrite = TRUE)
