x <- exp_decay(EuStockMarkets, 0.001)
y <- exp_decay(EuStockMarkets, 0.002)
z <- list(a = 1)
p_tidy <- bind_probs(x, y, z)

test_that("bind_probs rejects data that do not belong to `ffp`", {
  expect_equal(nrow(p_tidy), length(x) + length(y))
  expect_error(bind_probs(z, z))
})

test_that("bind_probs keeps the same n of rows and cols", {
  # dimension
  expect_equal(NCOL(p_tidy), NCOL(x) + 2L)
  expect_equal(NCOL(p_tidy), NCOL(y) + 2L)
  #class
  expect_type(p_tidy, "list")
  expect_s3_class(p_tidy, "tbl_df")
  expect_equal(purrr::map(p_tidy, class), list(rowid = "integer", probs = c("ffp", "vctrs_vctr"), key = "factor"))
})

