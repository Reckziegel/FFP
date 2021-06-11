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
  # type
  expect_type(smooth_numeric, "list")
  expect_s3_class(smooth_numeric, "tbl_df")
  expect_equal(purrr::map(smooth_numeric, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_numeric, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_numeric), NROW(xu))
  expect_equal(ncol(smooth_numeric), 2L)
  # is_prob
  expect_equal(sum(smooth_numeric$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_numeric$.p > 0))
})


# matrices
smooth_matu <- smoothing(xu_mat, lambda)
test_that("works on univariate matrices", {
  # type
  expect_type(smooth_matu, "list")
  expect_s3_class(smooth_numeric, "tbl_df")
  expect_equal(purrr::map(smooth_matu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_matu), NROW(xu))
  expect_equal(ncol(smooth_matu), 2L)
  # is_prob
  expect_equal(sum(smooth_matu$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_matu$.p > 0))
})

smooth_matm <- smoothing(xm_mat, lambda)
test_that("works on multivariate matrices", {
  # type
  expect_type(smooth_matm, "list")
  expect_s3_class(smooth_matm, "tbl_df")
  expect_equal(purrr::map(smooth_matm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_matm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_matm), NROW(xu))
  expect_equal(ncol(smooth_matm), 2L)
  # is_prob
  expect_equal(sum(smooth_matm$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_matm$.p > 0))
})

# ts
smooth_tsu <- smoothing(xu_ts, lambda)
test_that("works on univariate ts", {
  # type
  expect_type(smooth_tsu, "list")
  expect_s3_class(smooth_tsu, "tbl_df")
  expect_equal(purrr::map(smooth_tsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_tsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_tsu), NROW(xu))
  expect_equal(ncol(smooth_tsu), 2L)
  # is_prob
  expect_equal(sum(smooth_tsu$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_tsu$.p > 0))
})

smooth_tsm <- smoothing(xm_ts, lambda)
test_that("works on multivariate ts", {
  # type
  expect_type(smooth_tsm, "list")
  expect_s3_class(smooth_tsm, "tbl_df")
  expect_equal(purrr::map(smooth_tsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_tsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_tsm), NROW(xu))
  expect_equal(ncol(smooth_tsm), 2L)
  # is_prob
  expect_equal(sum(smooth_tsm$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_tsm$.p > 0))
})

# xts
smooth_xtsu <- smoothing(xu_xts, lambda)
test_that("works on univariate xts", {
  # type
  expect_type(smooth_xtsu, "list")
  expect_s3_class(smooth_xtsu, "tbl_df")
  expect_equal(purrr::map(smooth_xtsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_xtsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_xtsu), NROW(xu))
  expect_equal(ncol(smooth_xtsu), 2L)
  # is_prob
  expect_equal(sum(smooth_xtsu$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_xtsu$.p > 0))
})

smooth_xtsm <- smoothing(xm_xts, lambda)
test_that("works on multivariate xts", {
  # type
  expect_type(smooth_xtsm, "list")
  expect_s3_class(smooth_xtsm, "tbl_df")
  expect_equal(purrr::map(smooth_xtsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_xtsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_xtsm), NROW(xu))
  expect_equal(ncol(smooth_xtsm), 2L)
  # is_prob
  expect_equal(sum(smooth_xtsm$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_xtsm$.p > 0))
})


# data.frame
smooth_dfu <- smoothing(xu_df, lambda)
test_that("works on univariate data.frames", {
  # type
  expect_type(smooth_dfu, "list")
  expect_s3_class(smooth_dfu, "tbl_df")
  expect_equal(purrr::map(smooth_dfu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_dfu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_dfu), NROW(xu))
  expect_equal(ncol(smooth_dfu), 2L)
  # is_prob
  expect_equal(sum(smooth_dfu$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_dfu$.p > 0))
})

smooth_dfm <- smoothing(xm_df, lambda)
test_that("works on multivariate data.frames", {
  # type
  expect_type(smooth_dfm, "list")
  expect_s3_class(smooth_dfm, "tbl_df")
  expect_equal(purrr::map(smooth_dfm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_dfm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_dfm), NROW(xu))
  expect_equal(ncol(smooth_dfm), 2L)
  # is_prob
  expect_equal(sum(smooth_dfm$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_dfm$.p > 0))
})

# tbl
smooth_tblu <- smoothing(xu_tbl, lambda)
test_that("works on univariate tibbles", {
  # type
  expect_type(smooth_tblu, "list")
  expect_s3_class(smooth_tblu, "tbl_df")
  expect_equal(purrr::map(smooth_tblu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_tblu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_tblu), NROW(xu))
  expect_equal(ncol(smooth_tblu), 2L)
  # is_prob
  expect_equal(sum(smooth_tblu$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_tblu$.p > 0))
})

smooth_tblm <- smoothing(xm_tbl, lambda)
test_that("works on multivariate tibbles", {
  # type
  expect_type(smooth_tblm, "list")
  expect_s3_class(smooth_tblm, "tbl_df")
  expect_equal(purrr::map(smooth_tblm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(smooth_tblm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(smooth_tblm), NROW(xu))
  expect_equal(ncol(smooth_tblm), 2L)
  # is_prob
  expect_equal(sum(smooth_tblm$.p), 1, tolerance = 0.001)
  expect_true(all(smooth_tblm$.p > 0))
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
