set.seed(123)
# data
xu <- stats::rnorm(500)
xm <- matrix(stats::rnorm(1000), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 499, "day")

# Univariate
xu_vec <- xu
xu_mat <- as.matrix(xu)
xu_ts <- stats::as.ts(xu)
xu_xts <- xts::xts(xu, index)
xu_df <- as.data.frame(xu)
xu_tbl <- tibble::tibble(index = index, x = xu)

muu <- mean(xu)
volu <- var(xu)

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
  expect_error(kernel_entropy(xu_vec, sigma = volu)) # mu must be specified
})

test_that("error if NCOL(x) == 1 & `mean` or `sigma` are not a number of length 1", {
  expect_error(kernel_entropy(xu_vec, mean = c(muu, muu)))
  expect_error(kernel_entropy(xu_vec, mean = as.matrix(muu)))
  expect_error(kernel_entropy(xu_vec, sigma = c(volu, volu)))
  expect_error(kernel_entropy(xu_vec, sigma = as.matrix(volu)))
})

# TODO add tests for multivariate objects


# works on different classes ----------------------------------------------

# doubles
kernel_entropy_dbl <- kernel_entropy(xu_vec, muu, volu)
test_that("works on doubles", {
  # type
  expect_type(kernel_entropy_dbl, "list")
  expect_s3_class(kernel_entropy_dbl, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_dbl, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_dbl, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_dbl), NROW(xu))
  expect_equal(ncol(kernel_entropy_dbl), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_dbl$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_dbl$.p > 0))
})


# matrices
kernel_entropy_matu <- kernel_entropy(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  # type
  expect_type(kernel_entropy_matu, "list")
  expect_s3_class(kernel_entropy_matu, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_matu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_matu), NROW(xu))
  expect_equal(ncol(kernel_entropy_matu), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_matu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_matu$.p > 0))
})

kernel_entropy_matm <- kernel_entropy(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  # type
  expect_type(kernel_entropy_matm, "list")
  expect_s3_class(kernel_entropy_matm, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_matm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_matm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_matm), NROW(xu))
  expect_equal(ncol(kernel_entropy_matm), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_matm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_matm$.p > 0))
})


# ts
kernel_entropy_tsu <- kernel_entropy(xu_ts, muu, volu)
test_that("works on univariate ts", {
  # type
  expect_type(kernel_entropy_tsu, "list")
  expect_s3_class(kernel_entropy_tsu, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_tsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_tsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_tsu), NROW(xu))
  expect_equal(ncol(kernel_entropy_tsu), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_tsu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_tsu$.p > 0))
})

kernel_entropy_tsm <- kernel_entropy(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  # type
  expect_type(kernel_entropy_tsm, "list")
  expect_s3_class(kernel_entropy_tsm, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_tsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_tsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_tsm), NROW(xu))
  expect_equal(ncol(kernel_entropy_tsm), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_tsm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_tsm$.p > 0))
})

# xts
kernel_entropy_xtsu <- kernel_entropy(xu_xts, muu, volu)
test_that("works on univariate xts", {
  # type
  expect_type(kernel_entropy_xtsu, "list")
  expect_s3_class(kernel_entropy_xtsu, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_xtsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_xtsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_xtsu), NROW(xu))
  expect_equal(ncol(kernel_entropy_xtsu), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_xtsu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_xtsu$.p > 0))
})

kernel_entropy_xtsm <- kernel_entropy(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  # type
  expect_type(kernel_entropy_xtsm, "list")
  expect_s3_class(kernel_entropy_xtsm, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_xtsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_xtsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_xtsm), NROW(xu))
  expect_equal(ncol(kernel_entropy_xtsm), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_xtsm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_xtsm$.p > 0))
})

# data.frame
kernel_entropy_dfu <- kernel_entropy(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  # type
  expect_type(kernel_entropy_dfu, "list")
  expect_s3_class(kernel_entropy_dfu, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_dfu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_dfu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_dfu), NROW(xu))
  expect_equal(ncol(kernel_entropy_dfu), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_dfu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_dfu$.p > 0))
})

kernel_entropy_dfm <- kernel_entropy(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  # type
  expect_type(kernel_entropy_dfm, "list")
  expect_s3_class(kernel_entropy_dfm, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_dfm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_dfm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_dfm), NROW(xu))
  expect_equal(ncol(kernel_entropy_dfm), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_dfm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_dfm$.p > 0))
})

# tbl
kernel_entropy_tblu <- kernel_entropy(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_entropy_tblu, "list")
  expect_s3_class(kernel_entropy_tblu, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_tblu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_tblu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_tblu), NROW(xu))
  expect_equal(ncol(kernel_entropy_tblu), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_tblu$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_tblu$.p > 0))
})

kernel_entropy_tblm <- kernel_entropy(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_entropy_tblm, "list")
  expect_s3_class(kernel_entropy_tblm, "tbl_df")
  expect_equal(purrr::map(kernel_entropy_tblm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(kernel_entropy_tblm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(kernel_entropy_tblm), NROW(xu))
  expect_equal(ncol(kernel_entropy_tblm), 2L)
  # is_prob
  expect_equal(sum(kernel_entropy_tblm$.p), 1, tolerance = 0.001)
  expect_true(all(kernel_entropy_tblm$.p > 0))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  # univariate
  expect_equal(kernel_entropy_tblu, kernel_entropy_matu)
  expect_equal(kernel_entropy_dfu, kernel_entropy_tblu)
  # multivariate
  expect_equal(kernel_entropy_tblm, kernel_entropy_matm)
  expect_equal(kernel_entropy_dfm, kernel_entropy_tsm)
})
