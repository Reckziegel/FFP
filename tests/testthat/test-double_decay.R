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
  expect_equal(as.double(double_decay_tblu), as.double(double_decay_matu), tolerance = 0.0001)
  expect_equal(as.double(double_decay_dfu), as.double(double_decay_tblu), tolerance = 0.0001)
  # multivariate
  expect_equal(as.double(double_decay_tsm), as.double(double_decay_matm), tolerance = 0.0001)
  expect_equal(as.double(double_decay_xtsm), as.double(double_decay_tblm), tolerance = 0.0001)
})


# Double-decay moments ----------------------------------------------------

test_that("DoubleDecay preserves the legacy moment construction", {
  x <- matrix(
    c(
      0.01,  0.02,
      -0.02,  0.01,
      0.03, -0.01,
      -0.01,  0.03,
      0.02,  0.01,
      0.00, -0.02
    ),
    ncol = 2,
    byrow = TRUE
  )

  decay_low <- 0.0055
  decay_high <- 0.0166

  n_scenarios <- nrow(x)
  ages <- n_scenarios - seq_len(n_scenarios)

  correlation_weights <- exp(
    -decay_low * ages
  )

  correlation_weights <- (
    correlation_weights /
      sum(correlation_weights)
  )

  correlation_second_moment <- crossprod(
    x,
    sweep(
      x,
      MARGIN = 1,
      STATS = correlation_weights,
      FUN = "*"
    )
  )

  expected_correlation <- stats::cov2cor(
    correlation_second_moment
  )

  volatility_weights <- exp(
    -decay_high * ages
  )

  volatility_weights <- (
    volatility_weights /
      sum(volatility_weights)
  )

  volatility_second_moment <- crossprod(
    x,
    sweep(
      x,
      MARGIN = 1,
      STATS = volatility_weights,
      FUN = "*"
    )
  )

  expected_volatility <- sqrt(
    diag(volatility_second_moment)
  )

  expected_covariance <- (
    diag(expected_volatility) %*%
      expected_correlation %*%
      diag(expected_volatility)
  )

  result <- DoubleDecay(
    x = x,
    decay_low = decay_low,
    decay_high = decay_high
  )

  expect_equal(
    as.double(result$m),
    c(0, 0),
    tolerance = 1e-12
  )

  expect_equal(
    result$s,
    expected_covariance,
    tolerance = 1e-12
  )
})


test_that("DoubleDecay uses the FFP 2.0 exponential-decay core", {
  x <- xm_mat

  moments <- DoubleDecay(
    x = x,
    decay_low = slow,
    decay_high = fast
  )

  correlation_probabilities <- exp_decay_probabilities(
    n_scenarios = nrow(x),
    half_life = log(2) / slow
  )

  correlation_second_moment <- crossprod(
    x,
    sweep(
      x,
      MARGIN = 1,
      STATS = correlation_probabilities,
      FUN = "*"
    )
  )

  correlation <- stats::cov2cor(
    correlation_second_moment
  )

  volatility_probabilities <- exp_decay_probabilities(
    n_scenarios = nrow(x),
    half_life = log(2) / fast
  )

  volatility_second_moment <- crossprod(
    x,
    sweep(
      x,
      MARGIN = 1,
      STATS = volatility_probabilities,
      FUN = "*"
    )
  )

  volatility <- sqrt(
    diag(volatility_second_moment)
  )

  expected_covariance <- (
    diag(volatility) %*%
      correlation %*%
      diag(volatility)
  )

  expect_identical(
    moments$s,
    expected_covariance
  )
})


# Posterior moments -------------------------------------------------------

test_that("double_decay matches its target posterior moments", {
  result <- double_decay(
    xm_mat,
    slow = slow,
    fast = fast
  )

  probabilities <- as.double(result)

  target <- DoubleDecay(
    x = xm_mat,
    decay_low = slow,
    decay_high = fast
  )

  actual_mean <- drop(
    crossprod(
      probabilities,
      xm_mat
    )
  )

  centered <- sweep(
    xm_mat,
    MARGIN = 2,
    STATS = actual_mean,
    FUN = "-"
  )

  actual_covariance <- crossprod(
    centered,
    sweep(
      centered,
      MARGIN = 1,
      STATS = probabilities,
      FUN = "*"
    )
  )

  expect_equal(
    actual_mean,
    as.double(target$m),
    tolerance = 1e-8
  )

  expect_equal(
    actual_covariance,
    target$s,
    tolerance = 1e-8
  )
})


test_that("double_decay returns valid probabilities", {
  result <- double_decay(
    xm_mat,
    slow = slow,
    fast = fast
  )

  probabilities <- as.double(result)

  expect_true(
    all(is.finite(probabilities))
  )

  expect_true(
    all(probabilities >= 0)
  )

  expect_equal(
    sum(probabilities),
    1,
    tolerance = 1e-10
  )
})


# Legacy metadata ---------------------------------------------------------

test_that("double_decay preserves legacy metadata", {
  result <- double_decay(
    xu,
    slow = slow,
    fast = fast
  )

  expect_identical(
    attr(
      result,
      "fn",
      exact = TRUE
    ),
    "double_decay"
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
    "double_decay"
  )
})


test_that("double_decay remains compatible with bind_probs", {
  result <- double_decay(
    xu,
    slow = slow,
    fast = fast
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
        "double_decay",
        as.character(bound$fn),
        fixed = TRUE
      )
    )
  )
})
