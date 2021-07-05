set.seed(123)
# data
xu <- stats::rnorm(500)
xm <- matrix(stats::rnorm(1000), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 499, "day")

slow  <- 0.0055
fast <- 0.0166

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

test_that("args `slow` and `fast` must specified", {
  expect_error(double_decay(x = xu_vec, slow = slow))   # decay_high must be specified
  expect_error(double_decay(x = xu_vec, fast = fast)) # decay_low must be specified
})

test_that("error if `slow` or `fast` are not a number of length 1", {
  expect_error(double_decay(xu_vec, slow = c(slow, slow), fast = fast))
  expect_error(double_decay(xu_vec, slow = as.matrix(slow), fast = fast))
  expect_error(double_decay(xu_vec, slow = slow, fast = c(slow, slow)))
  expect_error(double_decay(xu_vec, slow = slow, fast = as.matrix(slow)))
})


# works on different classes ----------------------------------------------

# doubles
double_decay_dbl <- double_decay(xu_vec, slow, fast)
test_that("works on doubles", {
  # type
  expect_type(double_decay_dbl, "double")
  expect_s3_class(double_decay_dbl, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_dbl), vctrs::vec_size(xu))
})


# matrices
double_decay_matu <- double_decay(xu_mat, slow, fast)
test_that("works on univariate matrices", {
  # type
  expect_type(double_decay_matu, "double")
  expect_s3_class(double_decay_matu, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_matu), vctrs::vec_size(xu))
})

double_decay_matm <- double_decay(xm_mat, slow, fast)
test_that("works on multivariate matrices", {
  # type
  expect_type(double_decay_matm, "double")
  expect_s3_class(double_decay_matm, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_matm), vctrs::vec_size(xu))
})

# ts
double_decay_tsu <- double_decay(xu_ts, slow, fast)
test_that("works on univariate ts", {
  # type
  expect_type(double_decay_tsu, "double")
  expect_s3_class(double_decay_tsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_tsu), vctrs::vec_size(xu))
})

double_decay_tsm <- double_decay(xm_ts, slow, fast)
test_that("works on multivariate ts", {
  # type
  expect_type(double_decay_tsm, "double")
  expect_s3_class(double_decay_tsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_tsm), vctrs::vec_size(xu))
})

# xts
double_decay_xtsu <- double_decay(xu_xts, slow, fast)
test_that("works on univariate xts", {
  # type
  expect_type(double_decay_xtsu, "double")
  expect_s3_class(double_decay_xtsu, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_xtsu), vctrs::vec_size(xu))
})

double_decay_xtsm <- double_decay(xm_xts, slow, fast)
test_that("works on multivariate xts", {
  # type
  expect_type(double_decay_xtsm, "double")
  expect_s3_class(double_decay_xtsm, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_xtsm), vctrs::vec_size(xu))
})

# data.frame
double_decay_dfu <- double_decay(xu_df, slow, fast)
test_that("works on univariate data.frames", {
  # type
  expect_type(double_decay_dfu, "double")
  expect_s3_class(double_decay_dfu, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_dfu), vctrs::vec_size(xu))
})

double_decay_dfm <- double_decay(xm_df, slow, fast)
test_that("works on multivariate data.frames", {
  # type
  expect_type(double_decay_dfm, "double")
  expect_s3_class(double_decay_dfm, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_dfm), vctrs::vec_size(xu))
})

# tbl
double_decay_tblu <- double_decay(xu_tbl, slow, fast)
test_that("works on univariate on tibbles", {
  # type
  expect_type(double_decay_tblu, "double")
  expect_s3_class(double_decay_tblu, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_tblu), vctrs::vec_size(xu))
})

double_decay_tblm <- double_decay(xm_tbl, slow, fast)
test_that("works on multivariate tibbles", {
  # type
  expect_type(double_decay_tblm, "double")
  expect_s3_class(double_decay_tblm, "ffp")
  # size
  expect_equal(vctrs::vec_size(double_decay_tblm), vctrs::vec_size(xu))
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
