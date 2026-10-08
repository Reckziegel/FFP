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
  expect_equal(as.double(kernel_normal_tblu), as.double(kernel_normal_matu))
  expect_equal(as.double(kernel_normal_dfu), as.double(kernel_normal_tblu))
  # multivariate
  expect_equal(as.double(kernel_normal_tblm), as.double(kernel_normal_matm))
  expect_equal(as.double(kernel_normal_dfm), as.double(kernel_normal_tsm))
})

# FFP 2.0 core equivalence ------------------------------------------------

test_that("univariate kernel_normal matches the FFP 2.0 core", {
  expected <- kernel_conditioning_probabilities(
    values = as.double(xu),
    target = muu,
    bandwidth = sqrt(volu)
  )

  expect_identical(
    as.double(kernel_normal_dbl),
    expected
  )
})


test_that("kernel_normal maps sigma to squared bandwidth", {
  sigma <- 4

  result <- kernel_normal(
    c(-2, 0, 2),
    mean = 0,
    sigma = sigma
  )

  expected <- kernel_conditioning_probabilities(
    values = c(-2, 0, 2),
    target = 0,
    bandwidth = 2
  )

  expect_identical(
    as.double(result),
    expected
  )
})


test_that("univariate kernel_normal preserves symmetry around the mean", {
  result <- kernel_normal(
    c(-2, -1, 0, 1, 2),
    mean = 0,
    sigma = 1
  )

  probabilities <- as.double(result)

  expect_equal(
    probabilities[[1]],
    probabilities[[5]]
  )

  expect_equal(
    probabilities[[2]],
    probabilities[[4]]
  )

  expect_gt(
    probabilities[[3]],
    probabilities[[2]]
  )
})


# Multivariate legacy behavior --------------------------------------------

test_that("multivariate kernel_normal preserves Gaussian kernel semantics", {
  x <- matrix(
    c(
      -1, -1,
      0,  0,
      1,  1
    ),
    ncol = 2,
    byrow = TRUE
  )

  mean <- c(0, 0)
  sigma <- diag(2)

  expected <- mvtnorm::dmvnorm(
    x = x,
    mean = mean,
    sigma = sigma
  )

  expected <- expected / sum(expected)

  result <- kernel_normal(
    x,
    mean = mean,
    sigma = sigma
  )

  expect_equal(
    as.double(result),
    expected
  )
})


# Legacy metadata ---------------------------------------------------------

test_that("kernel_normal preserves legacy metadata", {
  result <- kernel_normal(
    xu,
    mean = muu,
    sigma = volu
  )

  expect_identical(
    attr(
      result,
      "fn",
      exact = TRUE
    ),
    "kernel_normal"
  )

  user_call <- attr(
    result,
    "user_call",
    exact = TRUE
  )

  expect_true(
    is.call(user_call)
  )

  expect_match(
    paste(
      deparse(user_call),
      collapse = ""
    ),
    "kernel_normal"
  )
})


test_that("kernel_normal remains compatible with bind_probs", {
  result <- kernel_normal(
    xu,
    mean = muu,
    sigma = volu
  )

  bound <- bind_probs(result)

  expect_s3_class(
    bound,
    "tbl_df"
  )

  expect_equal(
    bound$probs,
    as.double(result)
  )

  expect_true(
    all(
      grepl(
        "kernel_normal",
        as.character(bound$fn),
        fixed = TRUE
      )
    )
  )
})
