test_that("hm_filter_on_poly results in previously created object.", {
  expect_equal(hm_filter_on_poly( hm1, polygn ), hm_filtered_on_polygon)
})
