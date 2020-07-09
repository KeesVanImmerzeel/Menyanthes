test_that("Reading dino *_1.csv file with hm_read_dino() results in previously created object.", {
  fname <- system.file("extdata","B38E0108001_1.csv",package="menyanthes")
  expect_equal(hm_read_dino( fname ), hm3)
})

test_that("Reading all export Dino *_1.csv files with measured heads in specified folder with hm_read_dino_path() results in previously created object.", {
  path <- system.file("extdata","Grondwaterstanden_Put",package="menyanthes")
  expect_equal(hm_read_dino_path( path ), hm4)
})

