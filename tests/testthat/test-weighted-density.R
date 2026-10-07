# weighted_density_bandwidth() --------------------------------------------

test_that("weighted_density_bandwidth() returns a positive finite bandwidth", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  bandwidth <- weighted_density_bandwidth(values)

  expect_length(
    bandwidth,
    1L
  )

  expect_true(
    is.finite(bandwidth)
  )

  expect_gt(
    bandwidth,
    0
  )
})


test_that("weighted_density_bandwidth() applies adjust multiplicatively", {
  values <- seq(
    -2,
    2,
    length.out = 100
  )

  baseline <- weighted_density_bandwidth(
    values,
    adjust = 1
  )

  adjusted <- weighted_density_bandwidth(
    values,
    adjust = 1.5
  )

  expect_equal(
    adjusted,
    1.5 * baseline
  )
})


test_that("weighted density rejects constant features", {
  values <- rep(
    0.02,
    100
  )

  expect_error(
    weighted_density_bandwidth(values),
    class = "ffp_error_degenerate_density"
  )
})


test_that("weighted density validates adjust", {
  values <- 1:10

  invalid_adjust <- list(
    0,
    -1,
    NA_real_,
    Inf,
    c(
      1,
      2
    ),
    "1"
  )

  purrr::walk(
    invalid_adjust,
    \(adjust) {
      expect_error(
        weighted_density_bandwidth(
          values,
          adjust = adjust
        ),
        class = "ffp_error_invalid_density_adjust"
      )
    }
  )
})


# weighted_density_grid() -------------------------------------------------

test_that("weighted_density_grid() builds a finite increasing grid", {
  values <- seq(
    -2,
    2,
    length.out = 100
  )

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth
  )

  expect_length(
    grid,
    512L
  )

  expect_true(
    all(is.finite(grid))
  )

  expect_true(
    all(diff(grid) > 0)
  )

  expect_lt(
    min(grid),
    min(values)
  )

  expect_gt(
    max(grid),
    max(values)
  )
})


test_that("weighted_density_grid() respects n", {
  values <- seq(
    -2,
    2,
    length.out = 100
  )

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth,
    n = 128
  )

  expect_length(
    grid,
    128L
  )
})


# weighted_density() ------------------------------------------------------

test_that("weighted_density() returns density on the supplied grid", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  probabilities <- rep(
    0.2,
    5
  )

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth,
    n = 100
  )

  result <- weighted_density(
    values = values,
    probabilities = probabilities,
    grid = grid,
    bandwidth = bandwidth
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_identical(
    names(result),
    c(
      "x",
      "density"
    )
  )

  expect_equal(
    result$x,
    grid
  )

  expect_true(
    all(result$density >= 0)
  )

  expect_true(
    all(is.finite(result$density))
  )
})


test_that("weighted_density() is linear in probability weights", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.30
  )

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth,
    n = 100
  )

  baseline <- weighted_density(
    values = values,
    probabilities = probabilities,
    grid = grid,
    bandwidth = bandwidth
  )

  scaled <- weighted_density(
    values = values,
    probabilities = 0.6 * probabilities,
    grid = grid,
    bandwidth = bandwidth
  )

  expect_equal(
    scaled$density,
    0.6 * baseline$density
  )
})


test_that("weighted_density() does not renormalize probabilities", {
  values <- c(
    -1,
    0,
    1
  )

  normalized <- c(
    0.2,
    0.3,
    0.5
  )

  unnormalized <- 0.4 * normalized

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth,
    n = 100
  )

  normalized_density <- weighted_density(
    values = values,
    probabilities = normalized,
    grid = grid,
    bandwidth = bandwidth
  )

  unnormalized_density <- weighted_density(
    values = values,
    probabilities = unnormalized,
    grid = grid,
    bandwidth = bandwidth
  )

  expect_equal(
    unnormalized_density$density,
    0.4 * normalized_density$density
  )
})


test_that("weighted_density_at() matches direct kernel evaluation", {
  values <- c(
    -1,
    0,
    1
  )

  probabilities <- c(
    0.2,
    0.3,
    0.5
  )

  bandwidth <- weighted_density_bandwidth(values)

  point <- 0.25

  result <- weighted_density_at(
    values = values,
    probabilities = probabilities,
    x = point,
    bandwidth = bandwidth
  )

  expected <- sum(
    probabilities *
      stats::dnorm(
        (point - values) / bandwidth
      )
  ) / bandwidth

  expect_equal(
    result,
    expected
  )
})


# weighted_distribution_densities() --------------------------------------

test_that("weighted_distribution_densities() builds fit distributions", {
  values <- seq(
    -2,
    2,
    length.out = 101
  )

  prior <- rep(
    1 / length(values),
    length(values)
  )

  full_confidence <- seq_along(values)
  full_confidence <- full_confidence /
    sum(full_confidence)

  probabilities <- tibble::tibble(
    prior = prior,
    full_confidence = full_confidence
  )

  result <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities
  )

  expect_identical(
    levels(result$distribution),
    c(
      "prior",
      "full_confidence"
    )
  )

  expect_identical(
    names(result),
    c(
      "distribution",
      "x",
      "density",
      "bandwidth"
    )
  )
})


test_that("weighted distributions use exactly the same grid", {
  values <- seq(
    -3,
    3,
    length.out = 101
  )

  prior <- rep(
    1 / length(values),
    length(values)
  )

  full_confidence <- rev(
    seq_along(values)
  )

  full_confidence <- full_confidence /
    sum(full_confidence)

  probabilities <- tibble::tibble(
    prior = prior,
    full_confidence = full_confidence
  )

  result <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities,
    n = 200
  )

  prior_density <- result |>
    dplyr::filter(
      .data$distribution == "prior"
    )

  full_density <- result |>
    dplyr::filter(
      .data$distribution == "full_confidence"
    )

  expect_equal(
    prior_density$x,
    full_density$x
  )

  expect_equal(
    prior_density$bandwidth,
    full_density$bandwidth
  )
})


test_that("weighted distributions add posterior when available", {
  values <- seq(
    -2,
    2,
    length.out = 101
  )

  prior <- rep(
    1 / length(values),
    length(values)
  )

  full_confidence <- seq_along(values)
  full_confidence <- full_confidence /
    sum(full_confidence)

  confidence <- 0.6

  posterior <- (
    1 - confidence
  ) * prior +
    confidence * full_confidence

  probabilities <- tibble::tibble(
    prior = prior,
    full_confidence = full_confidence,
    posterior = posterior
  )

  result <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities
  )

  expect_identical(
    levels(result$distribution),
    c(
      "prior",
      "full_confidence",
      "posterior"
    )
  )
})


test_that("pooled density is the exact mixture of component densities", {
  values <- seq(
    -3,
    3,
    length.out = 101
  )

  prior <- rep(
    1 / length(values),
    length(values)
  )

  full_confidence <- seq_along(values)
  full_confidence <- full_confidence /
    sum(full_confidence)

  confidence <- 0.35

  posterior <- (
    1 - confidence
  ) * prior +
    confidence * full_confidence

  probabilities <- tibble::tibble(
    prior = prior,
    full_confidence = full_confidence,
    posterior = posterior
  )

  result <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities,
    n = 200
  )

  prior_density <- result |>
    dplyr::filter(
      .data$distribution == "prior"
    )

  full_density <- result |>
    dplyr::filter(
      .data$distribution == "full_confidence"
    )

  posterior_density <- result |>
    dplyr::filter(
      .data$distribution == "posterior"
    )

  expected <- (
    1 - confidence
  ) * prior_density$density +
    confidence * full_density$density

  expect_equal(
    posterior_density$density,
    expected
  )
})


test_that("adjust is shared across all distributions", {
  values <- seq(
    -2,
    2,
    length.out = 101
  )

  probabilities <- tibble::tibble(
    prior = rep(
      1 / length(values),
      length(values)
    ),
    full_confidence = rep(
      1 / length(values),
      length(values)
    )
  )

  baseline <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities,
    adjust = 1
  )

  adjusted <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities,
    adjust = 1.5
  )

  expect_equal(
    unique(adjusted$bandwidth),
    1.5 * unique(baseline$bandwidth)
  )
})


# Validation --------------------------------------------------------------

test_that("weighted density rejects invalid values", {
  invalid_values <- list(
    numeric(),
    c(
      1,
      NA_real_,
      3
    ),
    c(
      1,
      Inf,
      3
    ),
    character()
  )

  purrr::walk(
    invalid_values,
    \(values) {
      expect_error(
        weighted_density_bandwidth(values),
        class = "ffp_error_invalid_density_values"
      )
    }
  )
})


test_that("weighted_density() rejects invalid probabilities", {
  values <- 1:3

  bandwidth <- weighted_density_bandwidth(values)

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth
  )

  expect_error(
    weighted_density(
      values = values,
      probabilities = c(
        0.5,
        0.5
      ),
      grid = grid,
      bandwidth = bandwidth
    ),
    class = "ffp_error_invalid_density_probabilities"
  )

  expect_error(
    weighted_density(
      values = values,
      probabilities = c(
        0.5,
        -0.1,
        0.6
      ),
      grid = grid,
      bandwidth = bandwidth
    ),
    class = "ffp_error_invalid_density_probabilities"
  )

  expect_error(
    weighted_density(
      values = values,
      probabilities = c(
        0.5,
        NA_real_,
        0.5
      ),
      grid = grid,
      bandwidth = bandwidth
    ),
    class = "ffp_error_invalid_density_probabilities"
  )
})


test_that("weighted distributions require prior and full confidence", {
  probabilities <- tibble::tibble(
    prior = rep(
      1 / 3,
      3
    )
  )

  expect_error(
    weighted_distribution_densities(
      values = 1:3,
      probabilities = probabilities
    ),
    class = "ffp_error_invalid_density_probability_table"
  )
})


test_that("weighted density validates n", {
  values <- 1:10

  bandwidth <- weighted_density_bandwidth(values)

  invalid_n <- list(
    1,
    0,
    -1,
    2.5,
    NA_real_,
    Inf,
    c(
      100,
      200
    )
  )

  purrr::walk(
    invalid_n,
    \(n) {
      expect_error(
        weighted_density_grid(
          values = values,
          bandwidth = bandwidth,
          n = n
        ),
        class = "ffp_error_invalid_density_n"
      )
    }
  )
})
