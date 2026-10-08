test_that("fit_to_moments returns a probability vector", {
  x <- matrix(
    c(
      -2, -1,
      -1,  1,
      0,  0,
      1, -1,
      2,  1,
      1,  2
    ),
    ncol = 2,
    byrow = TRUE
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.20,
    0.15,
    0.20
  )

  target_mean <- drop(
    crossprod(
      probabilities,
      x
    )
  )

  centered <- sweep(
    x,
    MARGIN = 2,
    STATS = target_mean,
    FUN = "-"
  )

  target_covariance <- crossprod(
    centered,
    sweep(
      centered,
      MARGIN = 1,
      STATS = probabilities,
      FUN = "*"
    )
  )

  result <- fit_to_moments(
    X = x,
    m = target_mean,
    S = target_covariance
  )

  expect_type(
    result,
    "double"
  )

  expect_length(
    result,
    nrow(x)
  )

  expect_equal(
    sum(result),
    1,
    tolerance = 1e-10
  )

  expect_true(
    all(result >= 0)
  )
})


test_that("fit_to_moments matches target mean and covariance", {
  x <- matrix(
    c(
      -2, -1,
      -1,  1,
      0,  0,
      1, -1,
      2,  1,
      1,  2
    ),
    ncol = 2,
    byrow = TRUE
  )

  reference_probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.20,
    0.15,
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

  target_covariance <- crossprod(
    centered,
    sweep(
      centered,
      MARGIN = 1,
      STATS = reference_probabilities,
      FUN = "*"
    )
  )

  result <- fit_to_moments(
    X = x,
    m = target_mean,
    S = target_covariance
  )

  actual_mean <- drop(
    crossprod(
      result,
      x
    )
  )

  actual_centered <- sweep(
    x,
    MARGIN = 2,
    STATS = actual_mean,
    FUN = "-"
  )

  actual_covariance <- crossprod(
    actual_centered,
    sweep(
      actual_centered,
      MARGIN = 1,
      STATS = result,
      FUN = "*"
    )
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


test_that("fit_to_moments uses the FFP 2.0 moment core", {
  x <- matrix(
    c(
      -2, -1,
      -1,  1,
      0,  0,
      1, -1,
      2,  1,
      1,  2
    ),
    ncol = 2,
    byrow = TRUE
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.20,
    0.15,
    0.20
  )

  target_mean <- drop(
    crossprod(
      probabilities,
      x
    )
  )

  centered <- sweep(
    x,
    MARGIN = 2,
    STATS = target_mean,
    FUN = "-"
  )

  target_covariance <- crossprod(
    centered,
    sweep(
      centered,
      MARGIN = 1,
      STATS = probabilities,
      FUN = "*"
    )
  )

  expected <- moment_entropy_probabilities(
    x = x,
    target_mean = target_mean,
    target_covariance = target_covariance
  )

  result <- fit_to_moments(
    X = x,
    m = target_mean,
    S = target_covariance
  )

  expect_identical(
    result,
    expected
  )
})


test_that("fit_to_moments supports univariate moments", {
  x <- matrix(
    c(
      -2,
      -1,
      0,
      1,
      2
    ),
    ncol = 1
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.30
  )

  target_mean <- sum(
    probabilities * x[, 1]
  )

  target_variance <- sum(
    probabilities *
      (x[, 1] - target_mean)^2
  )

  result <- fit_to_moments(
    X = x,
    m = target_mean,
    S = matrix(target_variance)
  )

  actual_mean <- sum(
    result * x[, 1]
  )

  actual_variance <- sum(
    result *
      (x[, 1] - actual_mean)^2
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
