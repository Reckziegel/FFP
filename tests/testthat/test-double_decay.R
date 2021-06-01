# data
set.seed(123)
x <- stats::rnorm(200)
mu <- mean(x)
vol <- var(x)
index <- seq(Sys.Date(), Sys.Date() + 199, "day")
low_decay  <- 0.0055
high_decay <- 0.0166

# classes
x_vec <- x
x_mat <- as.matrix(x)
x_ts <- stats::as.ts(x)
x_xts <- xts::xts(x, index)
x_df <- as.data.frame(x)
x_tbl <- tibble::tibble(index = index, x = x)

# condition must be specified ---------------------------------------------

test_that("args `decay_low` and `decay_high` must specified", {
  expect_error(double_decay(x = x_vec, decay_low = low_decay))   # decay_high must be specified
  expect_error(double_decay(x = x_vec, decay_high = high_decay)) # decay_low must be specified
})

test_that("error if `decay_low` or `decay_high` are not a number of length 1", {
  expect_error(double_decay(x_vec, decay_low = c(low_decay, low_decay), decay_high = high_decay))
  expect_error(double_decay(x_vec, decay_low = as.matrix(low_decay), decay_high = high_decay))
  expect_error(double_decay(x_vec, decay_low = low_decay, decay_high = c(low_decay, low_decay)))
  expect_error(double_decay(x_vec, decay_low = low_decay, low_decay = as.matrix(low_decay)))
})


# works on different classes ----------------------------------------------

# doubles
double_decay_dbl <- double_decay(x_vec, low_decay, high_decay)
test_that("works on doubles", {
  expect_type(double_decay_dbl, "double")
  expect_length(double_decay_dbl, length(x))
  expect_equal(sum(double_decay_dbl), 1, tolerance = 0.001)
})


# matrices
double_decay_mat <- double_decay(x_mat, low_decay, high_decay)
test_that("works on matrices", {
  expect_type(double_decay_mat, "double")
  expect_length(double_decay_mat, length(x))
  expect_equal(sum(double_decay_mat), 1, tolerance = 0.001)
})

# ts
double_decay_ts <- double_decay(x_ts, low_decay, high_decay)
test_that("works on ts", {
  expect_type(double_decay_ts, "double")
  expect_length(double_decay_ts, length(x))
  expect_equal(sum(double_decay_ts), 1, tolerance = 0.001)
})

# xts
double_decay_xts <- double_decay(x_xts, low_decay, high_decay)
test_that("works on xts", {
  expect_type(double_decay_xts, "double")
  expect_length(double_decay_xts, length(x))
  expect_equal(sum(double_decay_ts), 1, tolerance = 0.001)
})

# data.frame
double_decay_df <- double_decay(x_df, low_decay, high_decay)
test_that("works on data.frames", {
  expect_type(double_decay_df, "double")
  expect_length(double_decay_df, length(x))
  expect_equal(sum(double_decay_df), 1, tolerance = 0.001)
})

# tbl
double_decay_tbl <- double_decay(x_tbl, low_decay, high_decay)
test_that("works on tibbles", {
  expect_type(double_decay_tbl, "double")
  expect_length(double_decay_tbl, length(x))
  expect_equal(sum(double_decay_tbl), 1, tolerance = 0.001)
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  expect_equal(double_decay_tbl, double_decay_mat, tolerance = 0.0001)
  expect_equal(double_decay_df, double_decay_tbl, tolerance = 0.0001)
})
