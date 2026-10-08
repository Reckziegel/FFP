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
  expect_equal(as.double(kernel_entropy_tblu), as.double(kernel_entropy_matu))
  expect_equal(as.double(kernel_entropy_dfu), as.double(kernel_entropy_tblu))
  # multivariate
  expect_equal(as.double(kernel_entropy_tblm), as.double(kernel_entropy_matm))
  expect_equal(as.double(kernel_entropy_dfm), as.double(kernel_entropy_tsm))
})


# FFP 2.0 core ------------------------------------------------------------

test_that("mean-only kernel entropy matches the FFP 2.0 public workflow", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  target_mean <- 0.35

  legacy <- kernel_entropy(
    x,
    mean = target_mean
  )

  fit <- ffp_model(x) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x = x,
        target = target_mean
      )
    ) |>
    ffp_fit()

  expect_equal(
    as.double(legacy),
    fit$posterior,
    tolerance = 1e-8
  )
})


test_that("kernel entropy matches target mean and variance", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  reference_probabilities <- c(
    0.08,
    0.12,
    0.20,
    0.25,
    0.35
  )

  target_mean <- sum(
    reference_probabilities * x
  )

  target_variance <- sum(
    reference_probabilities *
      (x - target_mean)^2
  )

  result <- kernel_entropy(
    x,
    mean = target_mean,
    sigma = target_variance
  )

  probabilities <- as.double(result)

  actual_mean <- sum(
    probabilities * x
  )

  actual_variance <- sum(
    probabilities *
      (x - actual_mean)^2
  )

  expect_equal(
    sum(probabilities),
    1,
    tolerance = 1e-10
  )

  expect_equal(
    actual_mean,
    target_mean,
    tolerance = 1e-8
  )

  expect_equal(
    actual_variance,
    target_variance,
    tolerance = 1e-8
  )
})


test_that("multivariate kernel entropy matches target moments", {
  x <- matrix(
    c(
      -2.0, -1.0,
      -1.0,  0.5,
      0.0,  1.0,
      1.0, -1.0,
      2.0,  0.5,
      0.5,  2.0
    ),
    ncol = 2,
    byrow = TRUE
  )

  reference_probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.10,
    0.20
  )

  target_mean <- drop(
    crossprod(
      reference_probabilities,
      x
    )
  )

  centered <- sweep(
    x,
    MARGIN = 2,
    STATS = target_mean,
    FUN = "-"
  )

  weighted_centered <- sweep(
    centered,
    MARGIN = 1,
    STATS = reference_probabilities,
    FUN = "*"
  )

  target_covariance <- crossprod(
    centered,
    weighted_centered
  )

  result <- kernel_entropy(
    x,
    mean = target_mean,
    sigma = target_covariance
  )

  probabilities <- as.double(result)

  actual_mean <- drop(
    crossprod(
      probabilities,
      x
    )
  )

  actual_centered <- sweep(
    x,
    MARGIN = 2,
    STATS = actual_mean,
    FUN = "-"
  )

  actual_weighted_centered <- sweep(
    actual_centered,
    MARGIN = 1,
    STATS = probabilities,
    FUN = "*"
  )

  actual_covariance <- crossprod(
    actual_centered,
    actual_weighted_centered
  )

  expect_equal(
    actual_mean,
    target_mean,
    tolerance = 1e-8
  )

  expect_equal(
    actual_covariance,
    target_covariance,
    tolerance = 1e-8
  )
})


# Legacy metadata ---------------------------------------------------------

test_that("kernel_entropy preserves legacy metadata", {
  result <- kernel_entropy(
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
    "kernel_entropy"
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
    "kernel_entropy"
  )
})


test_that("kernel_entropy remains compatible with bind_probs", {
  result <- kernel_entropy(
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
        "kernel_entropy",
        as.character(bound$fn),
        fixed = TRUE
      )
    )
  )
})
