library(testthat)


test_that("test make_file", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")

})
