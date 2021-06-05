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
  expect_type(kernel_normal_dbl, "double")
  expect_length(kernel_normal_dbl, vctrs::vec_size(xu_vec))
  expect_equal(sum(kernel_normal_dbl), 1)
})


# matrices
kernel_normal_matu <- kernel_normal(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  expect_type(kernel_normal_matu, "double")
  expect_length(kernel_normal_matu, vctrs::vec_size(xu_mat))
  expect_equal(sum(kernel_normal_matu), 1)
})

kernel_normal_matm <- kernel_normal(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  expect_type(kernel_normal_matm, "double")
  expect_length(kernel_normal_matm, vctrs::vec_size(xm_mat))
  expect_equal(sum(kernel_normal_matm), 1)
})


# ts
kernel_normal_tsu <- kernel_normal(xu_ts, muu, volu)
test_that("works on univariate ts", {
  expect_type(kernel_normal_tsu, "double")
  expect_length(kernel_normal_tsu, vctrs::vec_size(xu_ts))
  expect_equal(sum(kernel_normal_tsu), 1)
})

kernel_normal_tsm <- kernel_normal(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  expect_type(kernel_normal_tsm, "double")
  expect_length(kernel_normal_tsm, vctrs::vec_size(xm_ts))
  expect_equal(sum(kernel_normal_tsm), 1)
})

# xts
kernel_normal_xtsu <- kernel_normal(xu_xts, muu, volu)
test_that("works on univariate xts", {
  expect_type(kernel_normal_xtsu, "double")
  expect_length(kernel_normal_xtsu, vctrs::vec_size(xu_xts))
  expect_equal(sum(kernel_normal_xtsu), 1)
})

kernel_normal_xtsm <- kernel_normal(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  expect_type(kernel_normal_xtsm, "double")
  expect_length(kernel_normal_xtsm, vctrs::vec_size(xm_xts))
  expect_equal(sum(kernel_normal_xtsm), 1)
})

# data.frame
kernel_normal_dfu <- kernel_normal(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  expect_type(kernel_normal_dfu, "double")
  expect_length(kernel_normal_dfu, vctrs::vec_size(xu_df))
  expect_equal(sum(kernel_normal_dfu), 1)
})

kernel_normal_dfm <- kernel_normal(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  expect_type(kernel_normal_dfm, "double")
  expect_length(kernel_normal_dfm, vctrs::vec_size(xm_df))
  expect_equal(sum(kernel_normal_dfm), 1)
})

# tbl
kernel_normal_tblu <- kernel_normal(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  expect_type(kernel_normal_tblu, "double")
  expect_length(kernel_normal_tblu, vctrs::vec_size(xu_tbl))
  expect_equal(sum(kernel_normal_tblu), 1)
})

kernel_normal_tblm <- kernel_normal(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  expect_type(kernel_normal_tblm, "double")
  expect_length(kernel_normal_tblm, vctrs::vec_size(xm_tbl))
  expect_equal(sum(kernel_normal_tblm), 1)
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
