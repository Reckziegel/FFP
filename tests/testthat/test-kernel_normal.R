set.seed(123)
# data
xu <- stats::rnorm(50)
xm <- matrix(stats::rnorm(100), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 49, "day")

# Univariate
xu_vec <- xu
xu_mat <- as.matrix(xu)
xu_ts <- stats::as.ts(xu)
xu_xts <- xts::xts(xu, index)
xu_df <- as.data.frame(xu)
xu_tbl <- tibble::tibble(index = index, x = xu)

muu <- mean(xu)
volu <- var(xu) / 0.02

# Multivariate
xm_vec <- xm
xm_mat <- as.matrix(xm)
xm_ts <- stats::as.ts(xm)
xm_xts <- xts::xts(xm, index)
xm_df <- as.data.frame(xm)
xm_tbl <- tibble::tibble(index = index, x = xm)

mum <- colMeans(xm)
volm <- cov(xm)

# condition must be specified ---------------------------------------------

test_that("args `mean` and `sigma` must specified", {
  expect_error(kernel_normal(xu_vec, mean = muu))   # sigma must be specified
  expect_error(kernel_normal(xu_vec, sigma = volu)) # mu must be specified
})

test_that("error if NCOL(x) == 1 & `mean` or `sigma` are not a number of length 1", {
  expect_error(kernel_normal(xu_vec, mean = c(muu, muu)))
  expect_error(kernel_normal(xu_vec, mean = as.matrix(muu)))
  expect_error(kernel_normal(xu_vec, sigma = c(volu, volu)))
  expect_error(kernel_normal(xu_vec, sigma = as.matrix(volu)))
})

# TODO add tests for multivariate objects


# works on different classes ----------------------------------------------

# doubles
kernel_normal_dbl <- kernel_normal(xu_vec, muu, volu)
test_that("works on doubles", {
  # type
  expect_type(kernel_normal_dbl, "list")
  expect_s3_class(kernel_normal_dbl, "tbl_df")
  expect_equal(purrr::map(kernel_normal_dbl, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_dbl, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_dbl), NROW(xu))
  expect_equal(ncol(kernel_normal_dbl), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_dbl$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_dbl$.p > 0))
})


# matrices
kernel_normal_matu <- kernel_normal(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  # type
  expect_type(kernel_normal_matu, "list")
  expect_s3_class(kernel_normal_matu, "tbl_df")
  expect_equal(purrr::map(kernel_normal_matu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_matu), NROW(xu))
  expect_equal(ncol(kernel_normal_matu), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_matu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_matu$.p > 0))
})

kernel_normal_matm <- kernel_normal(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  # type
  expect_type(kernel_normal_matm, "list")
  expect_s3_class(kernel_normal_matm, "tbl_df")
  expect_equal(purrr::map(kernel_normal_matm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_matm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_matm), NROW(xu))
  expect_equal(ncol(kernel_normal_matm), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_matm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_matm$.p > 0))
})


# ts
kernel_normal_tsu <- kernel_normal(xu_ts, muu, volu)
test_that("works on univariate ts", {
  # type
  expect_type(kernel_normal_tsu, "list")
  expect_s3_class(kernel_normal_tsu, "tbl_df")
  expect_equal(purrr::map(kernel_normal_tsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_tsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_tsu), NROW(xu))
  expect_equal(ncol(kernel_normal_tsu), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_tsu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_tsu$.p > 0))
})

kernel_normal_tsm <- kernel_normal(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  # type
  expect_type(kernel_normal_tsm, "list")
  expect_s3_class(kernel_normal_tsm, "tbl_df")
  expect_equal(purrr::map(kernel_normal_tsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_tsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_tsm), NROW(xu))
  expect_equal(ncol(kernel_normal_tsm), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_tsm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_tsm$.p > 0))
})

# xts
kernel_normal_xtsu <- kernel_normal(xu_xts, muu, volu)
test_that("works on univariate xts", {
  # type
  expect_type(kernel_normal_xtsu, "list")
  expect_s3_class(kernel_normal_xtsu, "tbl_df")
  expect_equal(purrr::map(kernel_normal_xtsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_xtsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_xtsu), NROW(xu))
  expect_equal(ncol(kernel_normal_xtsu), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_xtsu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_xtsu$.p > 0))
})

kernel_normal_xtsm <- kernel_normal(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  # type
  expect_type(kernel_normal_xtsm, "list")
  expect_s3_class(kernel_normal_xtsm, "tbl_df")
  expect_equal(purrr::map(kernel_normal_xtsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_xtsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_xtsm), NROW(xu))
  expect_equal(ncol(kernel_normal_xtsm), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_xtsm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_xtsm$.p > 0))
})

# data.frame
kernel_normal_dfu <- kernel_normal(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  # type
  expect_type(kernel_normal_dfu, "list")
  expect_s3_class(kernel_normal_dfu, "tbl_df")
  expect_equal(purrr::map(kernel_normal_dfu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_dfu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_dfu), NROW(xu))
  expect_equal(ncol(kernel_normal_dfu), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_dfu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_dfu$.p > 0))
})

kernel_normal_dfm <- kernel_normal(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  # type
  expect_type(kernel_normal_dfm, "list")
  expect_s3_class(kernel_normal_dfm, "tbl_df")
  expect_equal(purrr::map(kernel_normal_dfm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_dfm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_dfm), NROW(xu))
  expect_equal(ncol(kernel_normal_dfm), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_dfm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_dfm$.p > 0))
})

# tbl
kernel_normal_tblu <- kernel_normal(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_normal_tblu, "list")
  expect_s3_class(kernel_normal_tblu, "tbl_df")
  expect_equal(purrr::map(kernel_normal_tblu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_tblu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_tblu), NROW(xu))
  expect_equal(ncol(kernel_normal_tblu), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_tblu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_tblu$.p > 0))
})

kernel_normal_tblm <- kernel_normal(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_normal_tblm, "list")
  expect_s3_class(kernel_normal_tblm, "tbl_df")
  expect_equal(purrr::map(kernel_normal_tblm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_normal_tblm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_normal_tblm), NROW(xu))
  expect_equal(ncol(kernel_normal_tblm), 2L)
  # is_prob
  expect_equal(sum(kernel_normal_tblm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_normal_tblm$.p > 0))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  # univariate
  expect_equal(kernel_normal_tblu, kernel_normal_matu)
  expect_equal(kernel_normal_dfu, kernel_normal_tblu)
  # multivariate
  expect_equal(kernel_normal_tblm, kernel_normal_matm)
  expect_equal(kernel_normal_dfm, kernel_normal_tsm)
})
