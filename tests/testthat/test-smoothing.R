set.seed(123)
# data
xu <- stats::rnorm(50)
xm <- matrix(stats::rnorm(100), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 49, "day")
lambda <- 0.01

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

# condition must be specified ---------------------------------------------

test_that("lambda must specified", {
  expect_error(smoothing(xu))
})

test_that("error if lambda is not a number of length 1", {
  expect_error(smoothing(xu, c(lambda, lambda)))
  expect_error(smoothing(xu, as.matrix(lambda)))
})

# works on different classes ----------------------------------------------

# doubles
smooth_numeric <- smoothing(xu, lambda)
test_that("works on doubles", {
  expect_type(smooth_numeric, "double")
  expect_length(smooth_numeric, vctrs::vec_size(xu))
})


# matrices
smooth_matu <- smoothing(xu_mat, lambda)
test_that("works on univariate matrices", {
  expect_type(smooth_matu, "double")
  expect_length(smooth_matu, vctrs::vec_size(xu_mat))
})

smooth_matm <- smoothing(xm_mat, lambda)
test_that("works on multivariate matrices", {
  expect_type(smooth_matm, "double")
  expect_length(smooth_matm, vctrs::vec_size(xm_mat))
})

# ts
smooth_tsu <- smoothing(xu_ts, lambda)
test_that("works on univariate ts", {
  expect_type(smooth_tsu, "double")
  expect_length(smooth_tsu, vctrs::vec_size(xu_ts))
})

smooth_tsm <- smoothing(xm_ts, lambda)
test_that("works on multivariate ts", {
  expect_type(smooth_tsm, "double")
  expect_length(smooth_tsm, vctrs::vec_size(xm_ts))
})

# xts
smooth_xtsu <- smoothing(xu_xts, lambda)
test_that("works on univariate xts", {
  expect_type(smooth_xtsu, "double")
  expect_length(smooth_xtsu, vctrs::vec_size(xu_xts))
})

smooth_xtsm <- smoothing(xm_xts, lambda)
test_that("works on multivariate xts", {
  expect_type(smooth_xtsm, "double")
  expect_length(smooth_xtsm, vctrs::vec_size(xm_xts))
})


# data.frame
smooth_dfu <- smoothing(xu_df, lambda)
test_that("works on univariate data.frames", {
  expect_type(smooth_dfu, "double")
  expect_length(smooth_dfu, vctrs::vec_size(xu_df))
})

smooth_dfm <- smoothing(xm_df, lambda)
test_that("works on multivariate data.frames", {
  expect_type(smooth_dfm, "double")
  expect_length(smooth_dfm, vctrs::vec_size(xm_df))
})

# tbl
smooth_tblu <- smoothing(xu_tbl, lambda)
test_that("works on univariate tibbles", {
  expect_type(smooth_tblu, "double")
  expect_length(smooth_tblu, vctrs::vec_size(xu_tbl))
})

smooth_tblm <- smoothing(xm_tbl, lambda)
test_that("works on multivariate tibbles", {
  expect_type(smooth_tblm, "double")
  expect_length(smooth_tblm, vctrs::vec_size(xm_tbl))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  # univariate
  expect_equal(smooth_numeric, smooth_matu)
  expect_equal(smooth_tsu, smooth_tblu)
  # multivariate
  expect_equal(smooth_matm, smooth_xtsm)
  expect_equal(smooth_tsm, smooth_tblm)
})
