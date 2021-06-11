set.seed(123)
# data
xu <- stats::rnorm(500)
xm <- matrix(stats::rnorm(1000), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 499, "day")

low_decay  <- 0.0055
high_decay <- 0.0166

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

test_that("args `decay_low` and `decay_high` must specified", {
  expect_error(double_decay(x = xu_vec, decay_low = low_decay))   # decay_high must be specified
  expect_error(double_decay(x = xu_vec, decay_high = high_decay)) # decay_low must be specified
})

test_that("error if `decay_low` or `decay_high` are not a number of length 1", {
  expect_error(double_decay(xu_vec, decay_low = c(low_decay, low_decay), decay_high = high_decay))
  expect_error(double_decay(xu_vec, decay_low = as.matrix(low_decay), decay_high = high_decay))
  expect_error(double_decay(xu_vec, decay_low = low_decay, decay_high = c(low_decay, low_decay)))
  expect_error(double_decay(xu_vec, decay_low = low_decay, decay_high = as.matrix(low_decay)))
})


# works on different classes ----------------------------------------------

# doubles
double_decay_dbl <- double_decay(xu_vec, low_decay, high_decay)
test_that("works on doubles", {
  # type
  expect_type(double_decay_dbl, "list")
  expect_s3_class(double_decay_dbl, "tbl_df")
  expect_equal(purrr::map(double_decay_dbl, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_dbl, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_dbl), NROW(xu))
  expect_equal(ncol(double_decay_dbl), 2L)
  # is_prob
  expect_equal(sum(double_decay_dbl$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_dbl$.p > 0))
})


# matrices
double_decay_matu <- double_decay(xu_mat, low_decay, high_decay)
test_that("works on univariate matrices", {
  # type
  expect_type(double_decay_matu, "list")
  expect_s3_class(double_decay_matu, "tbl_df")
  expect_equal(purrr::map(double_decay_matu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_matu), NROW(xu))
  expect_equal(ncol(double_decay_matu), 2L)
  # is_prob
  expect_equal(sum(double_decay_matu$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_matu$.p > 0))
})

double_decay_matm <- double_decay(xm_mat, low_decay, high_decay)
test_that("works on multivariate matrices", {
  # type
  expect_type(double_decay_matm, "list")
  expect_s3_class(double_decay_matm, "tbl_df")
  expect_equal(purrr::map(double_decay_matm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_matm), NROW(xu))
  expect_equal(ncol(double_decay_matm), 2L)
  # is_prob
  expect_equal(sum(double_decay_matm$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_matm$.p > 0))
})

# ts
double_decay_tsu <- double_decay(xu_ts, low_decay, high_decay)
test_that("works on univariate ts", {
  # type
  expect_type(double_decay_tsu, "list")
  expect_s3_class(double_decay_tsu, "tbl_df")
  expect_equal(purrr::map(double_decay_tsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_tsu), NROW(xu))
  expect_equal(ncol(double_decay_tsu), 2L)
  # is_prob
  expect_equal(sum(double_decay_tsu$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_tsu$.p > 0))
})

double_decay_tsm <- double_decay(xm_ts, low_decay, high_decay)
test_that("works on multivariate ts", {
  # type
  expect_type(double_decay_tsm, "list")
  expect_s3_class(double_decay_tsm, "tbl_df")
  expect_equal(purrr::map(double_decay_tsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_tsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_tsm), NROW(xu))
  expect_equal(ncol(double_decay_tsm), 2L)
  # is_prob
  expect_equal(sum(double_decay_tsm$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_tsm$.p > 0))
})

# xts
double_decay_xtsu <- double_decay(xu_xts, low_decay, high_decay)
test_that("works on univariate xts", {
  # type
  expect_type(double_decay_xtsu, "list")
  expect_s3_class(double_decay_xtsu, "tbl_df")
  expect_equal(purrr::map(double_decay_xtsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_xtsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_xtsu), NROW(xu))
  expect_equal(ncol(double_decay_xtsu), 2L)
  # is_prob
  expect_equal(sum(double_decay_xtsu$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_xtsu$.p > 0))
})

double_decay_xtsm <- double_decay(xm_xts, low_decay, high_decay)
test_that("works on multivariate xts", {
  # type
  expect_type(double_decay_xtsm, "list")
  expect_s3_class(double_decay_xtsm, "tbl_df")
  expect_equal(purrr::map(double_decay_xtsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_xtsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_xtsm), NROW(xu))
  expect_equal(ncol(double_decay_xtsm), 2L)
  # is_prob
  expect_equal(sum(double_decay_xtsm$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_xtsm$.p > 0))
})

# data.frame
double_decay_dfu <- double_decay(xu_df, low_decay, high_decay)
test_that("works on univariate data.frames", {
  # type
  expect_type(double_decay_dfu, "list")
  expect_s3_class(double_decay_dfu, "tbl_df")
  expect_equal(purrr::map(double_decay_dfu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_dfu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_dfu), NROW(xu))
  expect_equal(ncol(double_decay_dfu), 2L)
  # is_prob
  expect_equal(sum(double_decay_dfu$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_dfu$.p > 0))
})

double_decay_dfm <- double_decay(xm_df, low_decay, high_decay)
test_that("works on multivariate data.frames", {
  # type
  expect_type(double_decay_dfm, "list")
  expect_s3_class(double_decay_dfm, "tbl_df")
  expect_equal(purrr::map(double_decay_dfm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_dfm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_dfm), NROW(xu))
  expect_equal(ncol(double_decay_dfm), 2L)
  # is_prob
  expect_equal(sum(double_decay_dfm$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_dfm$.p > 0))
})

# tbl
double_decay_tblu <- double_decay(xu_tbl, low_decay, high_decay)
test_that("works on univariate on tibbles", {
  # type
  expect_type(double_decay_tblu, "list")
  expect_s3_class(double_decay_tblu, "tbl_df")
  expect_equal(purrr::map(double_decay_tblu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_tblu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_tblu), NROW(xu))
  expect_equal(ncol(double_decay_tblu), 2L)
  # is_prob
  expect_equal(sum(double_decay_tblu$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_tblu$.p > 0))
})

double_decay_tblm <- double_decay(xm_tbl, low_decay, high_decay)
test_that("works on multivariate tibbles", {
  # type
  expect_type(double_decay_tblm, "list")
  expect_s3_class(double_decay_tblm, "tbl_df")
  expect_equal(purrr::map(double_decay_tblm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(double_decay_tblm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(double_decay_tblm), NROW(xu))
  expect_equal(ncol(double_decay_tblm), 2L)
  # is_prob
  expect_equal(sum(double_decay_tblm$.p), 1, tolerance = 0.001)
  expect_true(all(double_decay_tblm$.p > 0))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  # univariate
  expect_equal(double_decay_tblu, double_decay_matu, tolerance = 0.0001)
  expect_equal(double_decay_dfu, double_decay_tblu, tolerance = 0.0001)
  # multivariate
  expect_equal(double_decay_tsm, double_decay_matm, tolerance = 0.0001)
  expect_equal(double_decay_xtsm, double_decay_tblm, tolerance = 0.0001)
})
