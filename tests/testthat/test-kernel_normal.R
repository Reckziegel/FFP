# data
set.seed(123)
x <- stats::rnorm(100)
mu <- mean(x)
vol <- var(x) / 0.02
index <- seq(Sys.Date(), Sys.Date() + 99, "day")

# classes
x_vec <- x
x_mat <- as.matrix(x)
x_ts <- stats::as.ts(x)
x_xts <- xts::xts(x, index)
x_df <- as.data.frame(x)
x_tbl <- tibble::tibble(index = index, x = x)

# condition must be specified ---------------------------------------------

test_that("args `mean` and `sigma` must specified", {
  expect_error(kernel_normal(x_vec, mean = mu)) # sigma must be specified
  expect_error(kernel_normal(x_vec, sigma = vol))     # mu must be specified
})

test_that("error if `mean` or `sigma` are not a number of length 1", {
  expect_error(kernel_normal(x_vec, mean = c(lambda, lambda)))
  expect_error(kernel_normal(x_vec, mean = as.matrix(lambda)))
  expect_error(kernel_normal(x_vec, sigma = c(lambda, lambda)))
  expect_error(kernel_normal(x_vec, sigma = as.matrix(lambda)))
})


# works on different classes ----------------------------------------------

# doubles
kernel_normal_dbl <- kernel_normal(x_vec, mu, vol)
test_that("works on doubles", {
  expect_type(kernel_normal_dbl, "double")
  expect_length(kernel_normal_dbl, length(x))
  expect_equal(sum(kernel_normal_dbl), 1)
})


# matrices
kernel_normal_mat <- kernel_normal(x_mat, mu, vol)
test_that("works on matrices", {
  expect_type(kernel_normal_mat, "double")
  expect_length(kernel_normal_mat, length(x))
  expect_equal(sum(kernel_normal_mat), 1)
})

# ts
kernel_normal_ts <- kernel_normal(x_ts, mu, vol)
test_that("works on ts", {
  expect_type(kernel_normal_ts, "double")
  expect_length(kernel_normal_ts, length(x))
  expect_equal(sum(kernel_normal_ts), 1)
})

# xts
kernel_normal_xts <- kernel_normal(x_xts, mu, vol)
test_that("works on xts", {
  expect_type(kernel_normal_xts, "double")
  expect_length(kernel_normal_xts, length(x))
  expect_equal(sum(kernel_normal_ts), 1)
})

# data.frame
kernel_normal_df <- kernel_normal(x_df, mu, vol)
test_that("works on data.frames", {
  expect_type(kernel_normal_df, "double")
  expect_length(kernel_normal_df, length(x))
  expect_equal(sum(kernel_normal_df), 1)
})

# tbl
kernel_normal_tbl <- kernel_normal(x_tbl, mu, vol)
test_that("works on tibbles", {
  expect_type(kernel_normal_tbl, "double")
  expect_length(kernel_normal_tbl, length(x))
  expect_equal(sum(kernel_normal_df), 1)
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  expect_equal(kernel_normal_tbl, kernel_normal_mat)
  expect_equal(kernel_normal_df, kernel_normal_tbl)
})
