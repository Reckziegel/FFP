library(dplyr, warn.conflicts = FALSE)

ret <- diff(log(EuStockMarkets))
p <- exp_smoothing(ret, 0.001)
esu <- empirical_stats(ret[ , 1], p)
esm <- empirical_stats(ret, p, 0.05)

test_that("level must be a single double", {
  expect_error(empirical_stats(ret[ , 1], p, c(0.02, 0.05)))
  expect_error(empirical_stats(ret[ , 1], p, matrix(0.01)))
})

test_that("p must be from an `ffp` class", {
  expect_error(empirical_stats(ret[ , 1], as.double(p)))
})

test_that("`x` and `p` must have the same size", {
  expect_error(empirical_stats(EuStockMarkets, p))
})

test_that("works on univariate data", {
  # dimension
  expect_equal(NCOL(esu), 3L)
  expect_equal(NROW(esu), 6L)
  #class
  expect_type(esu, "list")
  expect_s3_class(esu, "tbl_df")
  expect_equal(
    purrr::map(esu, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("works on multivariate data", {
  # dimension
  expect_equal(NCOL(esm), 3L)
  expect_equal(NROW(esm), 24L)
  #class
  expect_type(esm, "list")
  expect_s3_class(esm, "tbl_df")
  expect_equal(
    purrr::map(esm, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("argument level impacts the VaR and CVaR", {
  lvl_0.01 <- empirical_stats(ret[ , 1], p, 0.01) %>%
    filter(stat == "VaR") %>%
    pull(value)
  lvl_0.05 <- empirical_stats(ret[ , 1], p, 0.05) %>%
    filter(stat == "VaR") %>%
    pull(value)
  lvl_0.10 <- empirical_stats(ret[ , 1], p, 0.10) %>%
    filter(stat == "VaR") %>%
    pull(value)

  expect_true(lvl_0.01 > lvl_0.05)
  expect_true(lvl_0.05 > lvl_0.10)

})
