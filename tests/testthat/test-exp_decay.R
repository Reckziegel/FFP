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
  expect_error(exp_decay(xu))
})

test_that("error if lambda is not a number of length 1", {
  expect_error(exp_decay(xu, c(lambda, lambda)))
  expect_error(exp_decay(xu, as.matrix(lambda)))
})

# works on different classes ----------------------------------------------

# doubles
smooth_numeric <- exp_decay(xu, lambda)
test_that("works on doubles", {
  # type
  expect_type(smooth_numeric, "double")
  expect_s3_class(smooth_numeric, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_numeric), vctrs::vec_size(xu))
})


# matrices
smooth_matu <- exp_decay(xu_mat, lambda)
test_that("works on univariate matrices", {
  # type
  expect_type(smooth_matu, "double")
  expect_s3_class(smooth_numeric, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_matu), vctrs::vec_size(xu))
})

smooth_matm <- exp_decay(xm_mat, lambda)
test_that("works on multivariate matrices", {
  # type
  expect_type(smooth_matm, "double")
  expect_s3_class(smooth_matm, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_matm), vctrs::vec_size(xu))
})

# ts
smooth_tsu <- exp_decay(xu_ts, lambda)
test_that("works on univariate ts", {
  # type
  expect_type(smooth_tsu, "double")
  expect_s3_class(smooth_tsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_tsu), vctrs::vec_size(xu))
})

smooth_tsm <- exp_decay(xm_ts, lambda)
test_that("works on multivariate ts", {
  # type
  expect_type(smooth_tsm, "double")
  expect_s3_class(smooth_tsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_tsm), vctrs::vec_size(xu))
})

# xts
smooth_xtsu <- exp_decay(xu_xts, lambda)
test_that("works on univariate xts", {
  # type
  expect_type(smooth_xtsu, "double")
  expect_s3_class(smooth_xtsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_xtsu), vctrs::vec_size(xu))
})

smooth_xtsm <- exp_decay(xm_xts, lambda)
test_that("works on multivariate xts", {
  # type
  expect_type(smooth_xtsm, "double")
  expect_s3_class(smooth_xtsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_xtsm), vctrs::vec_size(xu))
})


# data.frame
smooth_dfu <- exp_decay(xu_df, lambda)
test_that("works on univariate data.frames", {
  # type
  expect_type(smooth_dfu, "double")
  expect_s3_class(smooth_dfu, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_dfu), vctrs::vec_size(xu))
})

smooth_dfm <- exp_decay(xm_df, lambda)
test_that("works on multivariate data.frames", {
  # type
  expect_type(smooth_dfm, "double")
  expect_s3_class(smooth_dfm, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_dfm), vctrs::vec_size(xu))
})

# tbl
smooth_tblu <- exp_decay(xu_tbl, lambda)
test_that("works on univariate tibbles", {
  # type
  expect_type(smooth_tblu, "double")
  expect_s3_class(smooth_tblu, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_tblu), vctrs::vec_size(xu))
})

smooth_tblm <- exp_decay(xm_tbl, lambda)
test_that("works on multivariate tibbles", {
  # type
  expect_type(smooth_tblm, "double")
  expect_s3_class(smooth_tblm, "ffp")
  # size
  expect_equal(vctrs::vec_size(smooth_tblm), vctrs::vec_size(xu))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  # univariate
  expect_equal(as.double(smooth_numeric), as.double(smooth_matu))
  expect_equal(as.double(smooth_tsu), as.double(smooth_tblu))
  # multivariate
  expect_equal(as.double(smooth_matm), as.double(smooth_xtsm))
  expect_equal(as.double(smooth_tsm), as.double(smooth_tblm))
})
