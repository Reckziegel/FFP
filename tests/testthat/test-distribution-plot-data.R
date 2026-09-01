# Helpers -----------------------------------------------------------------

make_distribution_plot_fit <- function() {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    ),
    inflation = c(
      0.02,
      0.03,
      0.035,
      0.045,
      0.05
    ),
    recession = c(
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE
    )
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        equity,
        target = 0.02
      )
    ) |>
    ffp_fit()
}


make_distribution_plot_pool <- function(
    confidence = 0.6
) {
  make_distribution_plot_fit() |>
    ffp_opinion_pooling(
      confidence = confidence
    )
}


# Fit ---------------------------------------------------------------------

test_that("distribution plot data supports ffp_fit", {
  fit <- make_distribution_plot_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_identical(
    levels(result$distribution),
    c(
      "Prior",
      "Full confidence"
    )
  )

  expect_true(
    all(result$feature == "equity")
  )
})


test_that("fit distribution plot data has the expected structure", {
  fit <- make_distribution_plot_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  expect_identical(
    names(result),
    c(
      "feature",
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


test_that("fit distributions share exactly the same bins", {
  fit <- make_distribution_plot_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 4
  )

  prior <- result |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  full <- result |>
    dplyr::filter(
      .data$distribution == "Full confidence"
    )

  expect_equal(
    prior$lower,
    full$lower
  )

  expect_equal(
    prior$upper,
    full$upper
  )

  expect_equal(
    prior$width,
    full$width
  )

  expect_equal(
    prior$midpoint,
    full$midpoint
  )
})


test_that("fit distribution masses sum to one", {
  fit <- make_distribution_plot_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity"
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


test_that("fit distribution densities integrate to one", {
  fit <- make_distribution_plot_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity"
  )

  total_density <- result |>
    dplyr::group_by(
      .data$distribution
    ) |>
    dplyr::summarise(
      integral = sum(
        .data$density *
          .data$width
      ),
      .groups = "drop"
    )

  expect_equal(
    total_density$integral,
    c(
      1,
      1
    )
  )
})


# Opinion pooling ---------------------------------------------------------

test_that("distribution plot data supports opinion pooling", {
  pooled <- make_distribution_plot_pool()

  result <- prepare_distribution_plot_data(
    object = pooled,
    feature = "equity",
    bins = 3
  )

  expect_identical(
    levels(result$distribution),
    c(
      "Prior",
      "Full confidence",
      "Posterior"
    )
  )
})


test_that("opinion pool extends fit distribution data", {
  fit <- make_distribution_plot_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.6
  )

  fit_data <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  pooled_data <- prepare_distribution_plot_data(
    object = pooled,
    feature = "equity",
    bins = 3
  )

  pooled_base <- pooled_data |>
    dplyr::filter(
      .data$distribution %in% c(
        "Prior",
        "Full confidence"
      )
    ) |>
    droplevels()

  expect_equal(
    pooled_base,
    fit_data
  )
})


test_that("pooled density is the expected histogram mixture", {
  confidence <- 0.35

  pooled <- make_distribution_plot_pool(
    confidence = confidence
  )

  result <- prepare_distribution_plot_data(
    object = pooled,
    feature = "equity",
    bins = 3
  )

  prior <- result |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  full <- result |>
    dplyr::filter(
      .data$distribution == "Full confidence"
    )

  posterior <- result |>
    dplyr::filter(
      .data$distribution == "Posterior"
    )

  expected_density <- (
    1 - confidence
  ) * prior$density +
    confidence * full$density

  expected_mass <- (
    1 - confidence
  ) * prior$mass +
    confidence * full$mass

  expect_equal(
    posterior$density,
    expected_density
  )

  expect_equal(
    posterior$mass,
    expected_mass
  )
})


# Feature resolution ------------------------------------------------------

test_that("distribution plot data selects the requested feature", {
  fit <- make_distribution_plot_fit()

  equity <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  inflation <- prepare_distribution_plot_data(
    object = fit,
    feature = "inflation",
    bins = 3
  )

  expect_true(
    all(equity$feature == "equity")
  )

  expect_true(
    all(inflation$feature == "inflation")
  )

  expect_false(
    identical(
      equity$lower,
      inflation$lower
    )
  )
})


test_that("distribution plot data supports named matrix scenarios", {
  scenarios <- matrix(
    c(
      -0.08, 0.02,
      -0.03, 0.03,
      0.01, 0.035,
      0.04, 0.045,
      0.06, 0.05
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c(
        "equity",
        "inflation"
      )
    )
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_fit()

  result <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  expect_true(
    all(result$feature == "equity")
  )

  expect_identical(
    levels(result$distribution),
    c(
      "Prior",
      "Full confidence"
    )
  )
})


test_that("feature must be a single non-empty string", {
  fit <- make_distribution_plot_fit()

  invalid_features <- list(
    character(),
    c(
      "equity",
      "inflation"
    ),
    NA_character_,
    "",
    1,
    matrix("equity")
  )

  purrr::walk(
    invalid_features,
    \(feature) {
      expect_error(
        prepare_distribution_plot_data(
          object = fit,
          feature = feature
        ),
        class = "ffp_error_invalid_plot_feature"
      )
    }
  )
})


test_that("unknown features are rejected", {
  fit <- make_distribution_plot_fit()

  expect_error(
    prepare_distribution_plot_data(
      object = fit,
      feature = "rates"
    ),
    class = "ffp_error_unknown_plot_feature"
  )
})


test_that("non-numeric features are rejected", {
  fit <- make_distribution_plot_fit()

  expect_error(
    prepare_distribution_plot_data(
      object = fit,
      feature = "recession"
    ),
    class = "ffp_error_non_numeric_plot_feature"
  )
})


test_that("unnamed scenario features are rejected", {
  scenarios <- matrix(
    c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    ),
    ncol = 1
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_fit()

  expect_error(
    prepare_distribution_plot_data(
      object = fit,
      feature = "equity"
    ),
    class = "ffp_error_unnamed_plot_features"
  )
})


# Bins --------------------------------------------------------------------

test_that("distribution plot data passes bins to histogram construction", {
  fit <- make_distribution_plot_fit()

  few_bins <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 2
  )

  many_bins <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 8
  )

  n_few <- dplyr::n_distinct(
    few_bins$bin
  )

  n_many <- dplyr::n_distinct(
    many_bins$bin
  )

  expect_gt(
    n_many,
    n_few
  )
})


test_that("distribution plot data preserves bin validation", {
  fit <- make_distribution_plot_fit()

  expect_error(
    prepare_distribution_plot_data(
      object = fit,
      feature = "equity",
      bins = 0
    ),
    class = "ffp_error_invalid_histogram_bins"
  )
})


# Object validation -------------------------------------------------------

test_that("distribution plot data rejects unsupported objects", {
  expect_error(
    prepare_distribution_plot_data(
      object = list(),
      feature = "equity"
    ),
    class = "ffp_error_invalid_distribution_plot"
  )
})


test_that("distribution plot preparation does not modify the object", {
  fit <- make_distribution_plot_fit()
  original <- fit

  prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 3
  )

  expect_identical(
    fit,
    original
  )
})
