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
  expect_type(double_decay_dbl, "double")
  expect_length(double_decay_dbl, vctrs::vec_size(xu_vec))
  expect_equal(sum(double_decay_dbl), 1, tolerance = 0.001)
})


# matrices
double_decay_matu <- double_decay(xu_mat, low_decay, high_decay)
test_that("works on univariate matrices", {
  expect_type(double_decay_matu, "double")
  expect_length(double_decay_matu, vctrs::vec_size(xu_mat))
  expect_equal(sum(double_decay_matu), 1, tolerance = 0.001)
})

double_decay_matm <- double_decay(xm_mat, low_decay, high_decay)
test_that("works on multivariate matrices", {
  expect_type(double_decay_matm, "double")
  expect_length(double_decay_matm, vctrs::vec_size(xm_mat))
  expect_equal(sum(double_decay_matm), 1, tolerance = 0.001)
})

# ts
double_decay_tsu <- double_decay(xu_ts, low_decay, high_decay)
test_that("works on univariate ts", {
  expect_type(double_decay_tsu, "double")
  expect_length(double_decay_tsu, vctrs::vec_size(xu_ts))
  expect_equal(sum(double_decay_tsu), 1, tolerance = 0.001)
})

double_decay_tsm <- double_decay(xm_ts, low_decay, high_decay)
test_that("works on multivariate ts", {
  expect_type(double_decay_tsm, "double")
  expect_length(double_decay_tsm, vctrs::vec_size(xm_ts))
  expect_equal(sum(double_decay_tsm), 1, tolerance = 0.001)
})

# xts
double_decay_xtsu <- double_decay(xu_xts, low_decay, high_decay)
test_that("works on univariate xts", {
  expect_type(double_decay_xtsu, "double")
  expect_length(double_decay_xtsu, vctrs::vec_size(xu_xts))
  expect_equal(sum(double_decay_xtsu), 1, tolerance = 0.001)
})

double_decay_xtsm <- double_decay(xm_xts, low_decay, high_decay)
test_that("works on multivariate xts", {
  expect_type(double_decay_xtsm, "double")
  expect_length(double_decay_xtsm, vctrs::vec_size(xm_xts))
  expect_equal(sum(double_decay_xtsm), 1, tolerance = 0.001)
})

# data.frame
double_decay_dfu <- double_decay(xu_df, low_decay, high_decay)
test_that("works on univariate data.frames", {
  expect_type(double_decay_dfu, "double")
  expect_length(double_decay_dfu, vctrs::vec_size(xu_df))
  expect_equal(sum(double_decay_dfu), 1, tolerance = 0.001)
})

double_decay_dfm <- double_decay(xm_df, low_decay, high_decay)
test_that("works on multivariate data.frames", {
  expect_type(double_decay_dfm, "double")
  expect_length(double_decay_dfm, vctrs::vec_size(xm_df))
  expect_equal(sum(double_decay_dfm), 1, tolerance = 0.001)
})

# tbl
double_decay_tblu <- double_decay(xu_tbl, low_decay, high_decay)
test_that("works on univariate on tibbles", {
  expect_type(double_decay_tblu, "double")
  expect_length(double_decay_tblu, vctrs::vec_size(xu_tbl))
  expect_equal(sum(double_decay_tblu), 1, tolerance = 0.001)
})

double_decay_tblm <- double_decay(xm_tbl, low_decay, high_decay)
test_that("works on multivariate tibbles", {
  expect_type(double_decay_tblm, "double")
  expect_length(double_decay_tblm, vctrs::vec_size(xm_tbl))
  expect_equal(sum(double_decay_tblm), 1, tolerance = 0.001)
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
