test_that("load_data works", {

  result <- load_data()

  expect_s3_class(result, "tbl_df")

  expect_equal(nrow(result), 11157)
  expect_equal(ncol(result), 21)
})
