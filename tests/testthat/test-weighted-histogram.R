# weighted_histogram_breaks() ---------------------------------------------

test_that("weighted_histogram_breaks() uses finite increasing breaks", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  breaks <- weighted_histogram_breaks(values)

  expect_true(
    is.numeric(breaks)
  )

  expect_true(
    all(is.finite(breaks))
  )

  expect_true(
    all(diff(breaks) > 0)
  )

  expect_lte(
    min(breaks),
    min(values)
  )

  expect_gte(
    max(breaks),
    max(values)
  )
})


test_that("weighted_histogram_breaks() accepts an explicit bin count", {
  values <- seq(
    -2,
    2,
    length.out = 100
  )

  breaks <- weighted_histogram_breaks(
    values,
    bins = 10
  )

  expect_true(
    length(breaks) >= 2L
  )

  expect_true(
    all(diff(breaks) > 0)
  )

  expect_lte(
    min(breaks),
    min(values)
  )

  expect_gte(
    max(breaks),
    max(values)
  )
})


test_that("weighted_histogram_breaks() handles constant values", {
  values <- rep(
    0.02,
    100
  )

  breaks <- weighted_histogram_breaks(values)

  expect_true(
    length(breaks) >= 2L
  )

  expect_true(
    all(diff(breaks) > 0)
  )

  expect_lte(
    min(breaks),
    0.02
  )

  expect_gte(
    max(breaks),
    0.02
  )
})


test_that("weighted_histogram_breaks() validates bins", {
  values <- 1:10

  invalid_bins <- list(
    0,
    -1,
    2.5,
    c(5, 10),
    NA_real_,
    Inf,
    "10"
  )

  purrr::walk(
    invalid_bins,
    \(bins) {
      expect_error(
        weighted_histogram_breaks(
          values,
          bins = bins
        ),
        class = "ffp_error_invalid_histogram_bins"
      )
    }
  )
})


# weighted_histogram() ----------------------------------------------------

test_that("weighted_histogram() preserves total probability mass", {
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

  breaks <- weighted_histogram_breaks(
    values,
    bins = 3
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    sum(result$mass),
    sum(probabilities)
  )
})


test_that("weighted_histogram() integrates density to total mass", {
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

  breaks <- weighted_histogram_breaks(
    values,
    bins = 3
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  integrated_density <- sum(
    result$density *
      result$width
  )

  expect_equal(
    integrated_density,
    sum(probabilities)
  )
})


test_that("weighted_histogram() does not renormalize probabilities", {
  values <- c(
    -1,
    0,
    1
  )

  probabilities <- c(
    0.1,
    0.2,
    0.3
  )

  breaks <- weighted_histogram_breaks(
    values,
    bins = 2
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    sum(result$mass),
    0.6
  )
})


test_that("weighted_histogram() aggregates repeated scenario values", {
  values <- c(
    0,
    0,
    0,
    1
  )

  probabilities <- c(
    0.10,
    0.20,
    0.30,
    0.40
  )

  breaks <- c(
    -0.5,
    0.5,
    1.5
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    result$mass,
    c(
      0.60,
      0.40
    )
  )
})


test_that("weighted_histogram() assigns boundary values only once", {
  values <- c(
    0,
    1,
    2
  )

  probabilities <- c(
    0.2,
    0.3,
    0.5
  )

  breaks <- c(
    0,
    1,
    2
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    sum(result$mass),
    1
  )

  expect_equal(
    result$mass,
    c(
      0.5,
      0.5
    )
  )
})


test_that("weighted_histogram() retains empty bins", {
  values <- c(
    0,
    3
  )

  probabilities <- c(
    0.4,
    0.6
  )

  breaks <- 0:3

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    nrow(result),
    3L
  )

  expect_equal(
    result$mass,
    c(
      0.4,
      0,
      0.6
    )
  )

  expect_equal(
    result$density[2],
    0
  )
})


test_that("weighted_histogram() returns the expected structure", {
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

  breaks <- c(
    -1,
    0,
    1
  )

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_identical(
    names(result),
    c(
      "bin",
      "lower",
      "upper",
      "midpoint",
      "width",
      "mass",
      "density"
    )
  )
})


test_that("weighted_histogram() handles constant features", {
  values <- rep(
    0.02,
    10
  )

  probabilities <- rep(
    0.1,
    10
  )

  breaks <- weighted_histogram_breaks(values)

  result <- weighted_histogram(
    values = values,
    probabilities = probabilities,
    breaks = breaks
  )

  expect_equal(
    sum(result$mass),
    1
  )

  expect_equal(
    sum(
      result$density *
        result$width
    ),
    1
  )
})


# weighted_distribution_histograms() --------------------------------------

test_that("weighted_distribution_histograms() builds fit distributions", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  probabilities <- tibble::tibble(
    scenario = seq_along(values),
    prior = rep(
      0.2,
      length(values)
    ),
    full_confidence = c(
      0.10,
      0.15,
      0.20,
      0.25,
      0.30
    )
  )

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 3
  )

  expect_identical(
    levels(result$distribution),
    c(
      "prior",
      "full_confidence"
    )
  )

  expect_setequal(
    unique(
      as.character(result$distribution)
    ),
    c(
      "prior",
      "full_confidence"
    )
  )
})


test_that("weighted distributions share exactly the same bins", {
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

  probabilities <- tibble::tibble(
    prior = prior,
    full_confidence = full_confidence
  )

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 12
  )

  prior_bins <- result |>
    dplyr::filter(
      .data$distribution == "prior"
    )

  full_bins <- result |>
    dplyr::filter(
      .data$distribution == "full_confidence"
    )

  expect_equal(
    prior_bins$lower,
    full_bins$lower
  )

  expect_equal(
    prior_bins$upper,
    full_bins$upper
  )

  expect_equal(
    prior_bins$width,
    full_bins$width
  )

  expect_equal(
    prior_bins$midpoint,
    full_bins$midpoint
  )
})


test_that("weighted distributions preserve each distribution mass", {
  values <- seq(
    -2,
    2,
    length.out = 50
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

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities
  )

  total_mass <- result |>
    dplyr::group_by(
      .data$distribution
    ) |>
    dplyr::summarise(
      mass = sum(.data$mass),
      .groups = "drop"
    )

  expect_equal(
    total_mass$mass,
    c(
      sum(prior),
      sum(full_confidence)
    )
  )
})


test_that("weighted distributions add posterior when available", {
  values <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  prior <- rep(
    0.2,
    length(values)
  )

  full_confidence <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.30
  )

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

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 3
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


test_that("pooled histogram is the mixture of histogram densities", {
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

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 15
  )

  prior_histogram <- result |>
    dplyr::filter(
      .data$distribution == "prior"
    )

  full_histogram <- result |>
    dplyr::filter(
      .data$distribution == "full_confidence"
    )

  posterior_histogram <- result |>
    dplyr::filter(
      .data$distribution == "posterior"
    )

  expected_density <- (
    1 - confidence
  ) * prior_histogram$density +
    confidence * full_histogram$density

  expected_mass <- (
    1 - confidence
  ) * prior_histogram$mass +
    confidence * full_histogram$mass

  expect_equal(
    posterior_histogram$density,
    expected_density
  )

  expect_equal(
    posterior_histogram$mass,
    expected_mass
  )
})


test_that("weighted distributions use one histogram for repeated values", {
  values <- c(
    -1,
    -1,
    0,
    0,
    1,
    1
  )

  probabilities <- tibble::tibble(
    prior = rep(
      1 / 6,
      6
    ),
    full_confidence = c(
      0.05,
      0.05,
      0.15,
      0.15,
      0.30,
      0.30
    )
  )

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 3
  )

  expect_identical(
    levels(result$distribution),
    c(
      "prior",
      "full_confidence"
    )
  )

  total_mass <- result |>
    dplyr::group_by(
      .data$distribution
    ) |>
    dplyr::summarise(
      mass = sum(.data$mass),
      .groups = "drop"
    )

  expect_equal(
    total_mass$mass,
    c(
      1,
      1
    )
  )
})


test_that("weighted distribution histogram has expected structure", {
  values <- 1:5

  probabilities <- tibble::tibble(
    prior = rep(
      0.2,
      5
    ),
    full_confidence = c(
      0.1,
      0.15,
      0.2,
      0.25,
      0.3
    )
  )

  result <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = 3
  )

  expect_identical(
    names(result),
    c(
      "distribution",
      "bin",
      "lower",
      "upper",
      "midpoint",
      "width",
      "mass",
      "density"
    )
  )
})


# Validation --------------------------------------------------------------

test_that("weighted histogram helpers reject invalid values", {
  expect_error(
    weighted_histogram_breaks(
      c(
        1,
        NA_real_,
        3
      )
    ),
    class = "ffp_error_invalid_histogram_values"
  )

  expect_error(
    weighted_histogram_breaks(
      c(
        1,
        Inf,
        3
      )
    ),
    class = "ffp_error_invalid_histogram_values"
  )

  expect_error(
    weighted_histogram_breaks(
      character()
    ),
    class = "ffp_error_invalid_histogram_values"
  )
})


test_that("weighted_histogram() rejects invalid probabilities", {
  values <- 1:3

  breaks <- c(
    0.5,
    1.5,
    2.5,
    3.5
  )

  expect_error(
    weighted_histogram(
      values = values,
      probabilities = c(
        0.5,
        0.5
      ),
      breaks = breaks
    ),
    class = "ffp_error_invalid_histogram_probabilities"
  )

  expect_error(
    weighted_histogram(
      values = values,
      probabilities = c(
        0.5,
        -0.1,
        0.6
      ),
      breaks = breaks
    ),
    class = "ffp_error_invalid_histogram_probabilities"
  )

  expect_error(
    weighted_histogram(
      values = values,
      probabilities = c(
        0.5,
        NA_real_,
        0.5
      ),
      breaks = breaks
    ),
    class = "ffp_error_invalid_histogram_probabilities"
  )
})


test_that("weighted_histogram() validates breaks", {
  values <- 1:3

  probabilities <- c(
    0.2,
    0.3,
    0.5
  )

  expect_error(
    weighted_histogram(
      values = values,
      probabilities = probabilities,
      breaks = c(
        0,
        2,
        1,
        3
      )
    ),
    class = "ffp_error_invalid_histogram_breaks"
  )

  expect_error(
    weighted_histogram(
      values = values,
      probabilities = probabilities,
      breaks = c(
        1.5,
        2.5
      )
    ),
    class = "ffp_error_invalid_histogram_breaks"
  )
})


test_that("weighted distributions require a data frame", {
  expect_error(
    weighted_distribution_histograms(
      values = 1:3,
      probabilities = matrix(
        rep(
          1 / 3,
          6
        ),
        ncol = 2
      )
    ),
    class = "ffp_error_invalid_histogram_probability_table"
  )
})


test_that("weighted distributions require one row per scenario", {
  probabilities <- tibble::tibble(
    prior = c(
      0.5,
      0.5
    ),
    full_confidence = c(
      0.4,
      0.6
    )
  )

  expect_error(
    weighted_distribution_histograms(
      values = 1:3,
      probabilities = probabilities
    ),
    class = "ffp_error_invalid_histogram_probability_table"
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
    weighted_distribution_histograms(
      values = 1:3,
      probabilities = probabilities
    ),
    class = "ffp_error_invalid_histogram_probability_table"
  )
})


test_that("weighted distributions validate every probability vector", {
  probabilities <- tibble::tibble(
    prior = rep(
      1 / 3,
      3
    ),
    full_confidence = c(
      0.5,
      -0.1,
      0.6
    )
  )

  expect_error(
    weighted_distribution_histograms(
      values = 1:3,
      probabilities = probabilities
    ),
    class = "ffp_error_invalid_histogram_probabilities"
  )
})
