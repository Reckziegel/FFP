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
  expect_type(kernel_entropy_dbl, "double")
  expect_s3_class(kernel_entropy_dbl, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_dbl), vctrs::vec_size(xu))
})

# matrices
kernel_entropy_matu <- kernel_entropy(xu_mat, muu, volu)
test_that("works on univariate matrices", {
  # type
  expect_type(kernel_entropy_matu, "double")
  expect_s3_class(kernel_entropy_matu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_matu), vctrs::vec_size(xu))
})

kernel_entropy_matm <- kernel_entropy(xm_mat, mum, volm)
test_that("works on multivariate matrices", {
  # type
  expect_type(kernel_entropy_matm, "double")
  expect_s3_class(kernel_entropy_matm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_matm), vctrs::vec_size(xu))
})


# ts
kernel_entropy_tsu <- kernel_entropy(xu_ts, muu, volu)
test_that("works on univariate ts", {
  # type
  expect_type(kernel_entropy_tsu, "double")
  expect_s3_class(kernel_entropy_tsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_tsu), vctrs::vec_size(xu))
})

kernel_entropy_tsm <- kernel_entropy(xm_ts, mum, volm)
test_that("works on multivariate ts", {
  # type
  expect_type(kernel_entropy_tsm, "double")
  expect_s3_class(kernel_entropy_tsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_tsm), vctrs::vec_size(xu))
})

# xts
kernel_entropy_xtsu <- kernel_entropy(xu_xts, muu, volu)
test_that("works on univariate xts", {
  # type
  expect_type(kernel_entropy_xtsu, "double")
  expect_s3_class(kernel_entropy_xtsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_xtsu), vctrs::vec_size(xu))
})

kernel_entropy_xtsm <- kernel_entropy(xm_xts, mum, volm)
test_that("works on multivariate xts", {
  # type
  expect_type(kernel_entropy_xtsm, "double")
  expect_s3_class(kernel_entropy_xtsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_xtsm), vctrs::vec_size(xu))
})

# data.frame
kernel_entropy_dfu <- kernel_entropy(xu_df, muu, volu)
test_that("works on univariate data.frames", {
  # type
  expect_type(kernel_entropy_dfu, "double")
  expect_s3_class(kernel_entropy_dfu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_dfu), vctrs::vec_size(xu))
})

kernel_entropy_dfm <- kernel_entropy(xm_df, mum, volm)
test_that("works on multivariate data.frames", {
  # type
  expect_type(kernel_entropy_dfm, "double")
  expect_s3_class(kernel_entropy_dfm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_dfm), vctrs::vec_size(xu))
})

# tbl
kernel_entropy_tblu <- kernel_entropy(xu_tbl, muu, volu)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_entropy_tblu, "double")
  expect_s3_class(kernel_entropy_tblu, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_tblu), vctrs::vec_size(xu))
})

kernel_entropy_tblm <- kernel_entropy(xm_tbl, mum, volm)
test_that("works on univariate tibbles", {
  # type
  expect_type(kernel_entropy_tblm, "double")
  expect_s3_class(kernel_entropy_tblm, "ffp")
  # size
  expect_equal(vctrs::vec_size(kernel_entropy_tblm), vctrs::vec_size(xu))
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
