# data
set.seed(123)
x <- stats::rnorm(50)
index <- seq(Sys.Date(), Sys.Date() + 49, "day")
lambda <- 0.05

# classes
x_vec <- x
x_mat <- as.matrix(x)
x_ts <- stats::as.ts(x)
x_xts <- xts::xts(x, index)
x_df <- as.data.frame(x)
x_tbl <- tibble::tibble(index = index, x = x)

# condition must be specified ---------------------------------------------

test_that("lambda must specified", {
  expect_error(smoothing(x))
})

test_that("error if lambda is not a number of length 1", {
  expect_error(smoothing(x, c(lambda, lambda)))
  expect_error(smoothing(x, as.matrix(lambda)))
})

# works on different classes ----------------------------------------------

# doubles
smooth_numeric <- smoothing(x, lambda)
test_that("works on doubles", {
  expect_type(smooth_numeric, "double")
  expect_length(smooth_numeric, length(x))
})


# matrices
smooth_mat <- smoothing(x_mat, lambda)
test_that("works on matrices", {
  expect_type(smooth_mat, "double")
  expect_length(smooth_mat, length(x))
})

# ts
smooth_ts <- smoothing(x_ts, lambda)
test_that("works on ts", {
  expect_type(smooth_ts, "double")
  expect_length(smooth_ts, length(x))
})

# xts
smooth_xts <- smoothing(x_xts, lambda)
test_that("works on xts", {
  expect_type(smooth_xts, "double")
  expect_length(smooth_xts, length(x))
})

# data.frame
smooth_df <- smoothing(x_df, lambda)
test_that("works on data.frames", {
  expect_type(smooth_df, "double")
  expect_length(smooth_df, length(x))
})

# tbl
smooth_tbl <- smoothing(x_tbl, lambda)
test_that("works on tibbles", {
  expect_type(smooth_tbl, "double")
  expect_length(smooth_tbl, length(x))
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  expect_equal(smooth_numeric, smooth_mat)
  expect_equal(smooth_ts, smooth_tbl)
})
