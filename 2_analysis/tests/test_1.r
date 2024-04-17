test_that("The cleaned datasets used in the analysis are of the expected size (fails otherwise)", {
  expect_equal(nrow(data_long_all),1800)
  expect_equal(nrow(data_long_market),1200)
  expect_equal(nrow(data_long_church),1200)
  expect_equal(nrow(data_spillover_all),1791)
  expect_equal(nrow(data_spillover_market),1194)
  expect_equal(nrow(data_spillover_church),1195)
})