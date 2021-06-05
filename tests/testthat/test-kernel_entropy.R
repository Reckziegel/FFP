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
  expect_type(kernel_entropy_dbl, "double")
  expect_length(kernel_entropy_dbl, vctrs::vec_size(xu_vec))
  expect_equal(sum(kernel_entropy_dbl), 1, tolerance = 0.001)
})


# matrices
kernel_entropy_matu <- kernel_entropy(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  expect_type(kernel_entropy_matu, "double")
  expect_length(kernel_entropy_matu, vctrs::vec_size(xu_mat))
  expect_equal(sum(kernel_entropy_matu), 1, tolerance = 0.001)
})

kernel_entropy_matm <- kernel_entropy(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  expect_type(kernel_entropy_matm, "double")
  expect_length(kernel_entropy_matm, vctrs::vec_size(xm_mat))
  expect_equal(sum(kernel_entropy_matm), 1, tolerance = 0.001)
})


# ts
kernel_entropy_tsu <- kernel_entropy(xu_ts, muu, volu)
test_that("works on univariate ts", {
  expect_type(kernel_entropy_tsu, "double")
  expect_length(kernel_entropy_tsu, vctrs::vec_size(xu_ts))
  expect_equal(sum(kernel_entropy_tsu), 1, tolerance = 0.001)
})

kernel_entropy_tsm <- kernel_entropy(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  expect_type(kernel_entropy_tsm, "double")
  expect_length(kernel_entropy_tsm, vctrs::vec_size(xm_ts))
  expect_equal(sum(kernel_entropy_tsm), 1, tolerance = 0.001)
})

# xts
kernel_entropy_xtsu <- kernel_entropy(xu_xts, muu, volu)
test_that("works on univariate xts", {
  expect_type(kernel_entropy_xtsu, "double")
  expect_length(kernel_entropy_xtsu, vctrs::vec_size(xu_xts))
  expect_equal(sum(kernel_entropy_xtsu), 1, tolerance = 0.001)
})

kernel_entropy_xtsm <- kernel_entropy(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  expect_type(kernel_entropy_xtsm, "double")
  expect_length(kernel_entropy_xtsm, vctrs::vec_size(xm_xts))
  expect_equal(sum(kernel_entropy_xtsm), 1, tolerance = 0.001)
})

# data.frame
kernel_entropy_dfu <- kernel_entropy(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  expect_type(kernel_entropy_dfu, "double")
  expect_length(kernel_entropy_dfu, vctrs::vec_size(xu_df))
  expect_equal(sum(kernel_entropy_dfu), 1, tolerance = 0.001)
})

kernel_entropy_dfm <- kernel_entropy(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  expect_type(kernel_entropy_dfm, "double")
  expect_length(kernel_entropy_dfm, vctrs::vec_size(xm_df))
  expect_equal(sum(kernel_entropy_dfm), 1, tolerance = 0.001)
})

# tbl
kernel_entropy_tblu <- kernel_entropy(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  expect_type(kernel_entropy_tblu, "double")
  expect_length(kernel_entropy_tblu, vctrs::vec_size(xu_tbl))
  expect_equal(sum(kernel_entropy_tblu), 1, tolerance = 0.001)
})

kernel_entropy_tblm <- kernel_entropy(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  expect_type(kernel_entropy_tblm, "double")
  expect_length(kernel_entropy_tblm, vctrs::vec_size(xm_tbl))
  expect_equal(sum(kernel_entropy_tblm), 1, tolerance = 0.001)
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
