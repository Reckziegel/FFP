set.seed(123)
# data
xu <- stats::rnorm(500)
xm <- matrix(stats::rnorm(1000), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 499, "day")
prob <- exp_decay(xu, 0.001)

# Univariate
xu_vec <- xu
xu_mat <- as.matrix(xu)
xu_ts <- stats::as.ts(xu)
xu_xts <- xts::xts(xu, index)
xu_df <- as.data.frame(xu)
xu_tbl <- tibble::tibble(index = index, x = xu)

# Multivariate
xm_vec <- xm
xm_mat <- as.matrix(xm)
xm_ts <- stats::as.ts(xm)
xm_xts <- xts::xts(xm, index)
xm_df <- as.data.frame(xm)
xm_tbl <- tibble::tibble(index = index, x = xm)

# works on different classes ----------------------------------------------

# doubles
bootstrap_dbl <- bootstrap_scenarios(xu_vec, prob, 5)
test_that("works on doubles", {
  # type
  expect_type(bootstrap_dbl, "list")
  expect_s3_class(bootstrap_dbl, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_dbl), 1L)
  expect_equal(nrow(bootstrap_dbl), 5L)
})

# matrices
bootstrap_matu <- bootstrap_scenarios(xu_mat, prob, 5)
test_that("works on univariate matrices", {
  # type
  expect_type(bootstrap_matu, "list")
  expect_s3_class(bootstrap_matu, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_matu), 1L)
  expect_equal(nrow(bootstrap_matu), 5L)
})

bootstrap_matm <- bootstrap_scenarios(xm_mat, prob, 5)
test_that("works on multivariate matrices", {
  # type
  expect_type(bootstrap_matm, "list")
  expect_s3_class(bootstrap_matm, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_matm), 2L)
  expect_equal(nrow(bootstrap_matm), 5L)
})

# ts
bootstrap_tsu <- bootstrap_scenarios(xu_ts, prob, 5)
test_that("works on univariate ts", {
  # type
  expect_type(bootstrap_tsu, "list")
  expect_s3_class(bootstrap_tsu, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_tsu), 1L)
  expect_equal(nrow(bootstrap_tsu), 5L)
})

bootstrap_tsm <- bootstrap_scenarios(xm_ts, prob, 5)
test_that("works on multivariate ts", {
  # type
  expect_type(bootstrap_tsm, "list")
  expect_s3_class(bootstrap_tsm, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_tsm), 2L)
  expect_equal(nrow(bootstrap_tsm), 5L)
})

# xts
bootstrap_xtsu <- bootstrap_scenarios(xu_xts, prob, 5)
test_that("works on univariate xts", {
  # type
  expect_type(bootstrap_xtsu, "list")
  expect_s3_class(bootstrap_xtsu, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_xtsu), 1L)
  expect_equal(nrow(bootstrap_xtsu), 5L)
})

bootstrap_xtsm <- bootstrap_scenarios(xm_xts, prob, 5)
test_that("works on multivariate xts", {
  # type
  expect_type(bootstrap_xtsm, "list")
  expect_s3_class(bootstrap_xtsm, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_xtsm), 2L)
  expect_equal(nrow(bootstrap_xtsm), 5L)
})

# data.frame
bootstrap_dfu <- bootstrap_scenarios(xu_df, prob, 5)
test_that("works on univariate data.frames", {
  # type
  expect_type(bootstrap_dfu, "list")
  expect_s3_class(bootstrap_dfu, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_dfu), 1L)
  expect_equal(nrow(bootstrap_dfu), 5L)
})

bootstrap_dfm <- bootstrap_scenarios(xm_df, prob, 5)
test_that("works on multivariate data.frames", {
  # type
  expect_type(bootstrap_dfm, "list")
  expect_s3_class(bootstrap_dfm, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_dfm), 2L)
  expect_equal(nrow(bootstrap_dfm), 5L)
})

# tbl
bootstrap_tblu <- bootstrap_scenarios(xu_tbl, prob, 5)
test_that("works on univariate on tibbles", {
  # type
  expect_type(bootstrap_tblu, "list")
  expect_s3_class(bootstrap_tblu, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_tblu), 1L)
  expect_equal(nrow(bootstrap_tblu), 5L)
})

bootstrap_tblm <- bootstrap_scenarios(xm_tbl, prob, 5)
test_that("works on multivariate tibbles", {
  # type
  expect_type(bootstrap_tblm, "list")
  expect_s3_class(bootstrap_tblm, "tbl_df")
  # size
  expect_equal(ncol(bootstrap_tblm), 2L)
  expect_equal(nrow(bootstrap_tblm), 5L)
})

