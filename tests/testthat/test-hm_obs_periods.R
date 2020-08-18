library(menyanthes)
test_that("hm_obs_periods results in previously created object.", {
  expect_equal(hm_obs_periods( hm1 ), obs_periods)
})
