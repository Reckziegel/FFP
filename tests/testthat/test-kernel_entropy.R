# data
set.seed(123)
x <- stats::rnorm(200)
mu <- mean(x)
vol <- var(x)
index <- seq(Sys.Date(), Sys.Date() + 199, "day")

# classes
x_vec <- x
x_mat <- as.matrix(x)
x_ts <- stats::as.ts(x)
x_xts <- xts::xts(x, index)
x_df <- as.data.frame(x)
x_tbl <- tibble::tibble(index = index, x = x)

# condition must be specified ---------------------------------------------

test_that("args `mean` and must specified", {
  expect_error(kernel_entropy(x_vec, sigma = vol))     # mu must be specified
})

test_that("error if `mean` or `sigma` are not a number of length 1", {
  expect_error(kernel_entropy(x_vec, mean = c(mu, mu)))
  expect_error(kernel_entropy(x_vec, mean = as.matrix(mu)))
  expect_error(kernel_entropy(x_vec, sigma = c(vol, vol)))
  expect_error(kernel_entropy(x_vec, sigma = as.matrix(vol)))
})


# works on different classes ----------------------------------------------

# doubles
kernel_entropy_dbl <- kernel_entropy(x_vec, mu, vol)
test_that("works on doubles", {
  expect_type(kernel_entropy_dbl, "double")
  expect_length(kernel_entropy_dbl, length(x))
  expect_equal(sum(kernel_entropy_dbl), 1, tolerance = 0.001)
})


# matrices
kernel_entropy_mat <- kernel_entropy(x_mat, mu, vol)
test_that("works on matrices", {
  expect_type(kernel_entropy_mat, "double")
  expect_length(kernel_entropy_mat, length(x))
  expect_equal(sum(kernel_entropy_mat), 1, tolerance = 0.001)
})

# ts
kernel_entropy_ts <- kernel_entropy(x_ts, mu, vol)
test_that("works on ts", {
  expect_type(kernel_entropy_ts, "double")
  expect_length(kernel_entropy_ts, length(x))
  expect_equal(sum(kernel_entropy_ts), 1, tolerance = 0.001)
})

# xts
kernel_entropy_xts <- kernel_entropy(x_xts, mu, vol)
test_that("works on xts", {
  expect_type(kernel_entropy_xts, "double")
  expect_length(kernel_entropy_xts, length(x))
  expect_equal(sum(kernel_entropy_ts), 1, tolerance = 0.001)
})

# data.frame
kernel_entropy_df <- kernel_entropy(x_df, mu, vol)
test_that("works on data.frames", {
  expect_type(kernel_entropy_df, "double")
  expect_length(kernel_entropy_df, length(x))
  expect_equal(sum(kernel_entropy_df), 1, tolerance = 0.001)
})

# tbl
kernel_entropy_tbl <- kernel_entropy(x_tbl, mu, vol)
test_that("works on tibbles", {
  expect_type(kernel_entropy_tbl, "double")
  expect_length(kernel_entropy_tbl, length(x))
  expect_equal(sum(kernel_entropy_tbl), 1, tolerance = 0.001)
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  expect_equal(kernel_entropy_tbl, kernel_entropy_mat, tolerance = 0.0001)
  expect_equal(kernel_entropy_df, kernel_entropy_tbl, tolerance = 0.0001)
})
