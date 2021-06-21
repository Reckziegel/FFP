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
  expect_type(kernel_normal_dbl, "double")
  expect_s3_class(kernel_normal_dbl, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_dbl), vctrs::vec_size(xu))
})


# matrices
kernel_normal_matu <- kernel_normal(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  # type
  expect_type(kernel_normal_matu, "double")
  expect_s3_class(kernel_normal_matu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_matu), vctrs::vec_size(xu))
})

kernel_normal_matm <- kernel_normal(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  # type
  expect_type(kernel_normal_matm, "double")
  expect_s3_class(kernel_normal_matm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_matm), vctrs::vec_size(xu))
})


# ts
kernel_normal_tsu <- kernel_normal(xu_ts, muu, volu)
test_that("works on univariate ts", {
  # type
  expect_type(kernel_normal_tsu, "double")
  expect_s3_class(kernel_normal_tsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_tsu), vctrs::vec_size(xu))
})

kernel_normal_tsm <- kernel_normal(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  # type
  expect_type(kernel_normal_tsm, "double")
  expect_s3_class(kernel_normal_tsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_tsm), vctrs::vec_size(xu))
})

# xts
kernel_normal_xtsu <- kernel_normal(xu_xts, muu, volu)
test_that("works on univariate xts", {
  # type
  expect_type(kernel_normal_xtsu, "double")
  expect_s3_class(kernel_normal_xtsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_xtsu), vctrs::vec_size(xu))
})

kernel_normal_xtsm <- kernel_normal(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  # type
  expect_type(kernel_normal_xtsm, "double")
  expect_s3_class(kernel_normal_xtsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_xtsm), vctrs::vec_size(xu))
})

# data.frame
kernel_normal_dfu <- kernel_normal(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  # type
  expect_type(kernel_normal_dfu, "double")
  expect_s3_class(kernel_normal_dfu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_dfu), vctrs::vec_size(xu))
})

kernel_normal_dfm <- kernel_normal(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  # type
  expect_type(kernel_normal_dfm, "double")
  expect_s3_class(kernel_normal_dfm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_dfm), vctrs::vec_size(xu))
})

# tbl
kernel_normal_tblu <- kernel_normal(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_normal_tblu, "double")
  expect_s3_class(kernel_normal_tblu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_tblu), vctrs::vec_size(xu))
})

kernel_normal_tblm <- kernel_normal(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_normal_tblm, "double")
  expect_s3_class(kernel_normal_tblm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_normal_tblm), vctrs::vec_size(xu))
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
