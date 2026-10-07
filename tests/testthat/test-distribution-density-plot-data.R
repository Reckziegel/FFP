# Helpers -----------------------------------------------------------------

make_density_plot_data_fit <- function() {
  scenarios <- data.frame(
    equity = c(
      -0.10,
      -0.07,
      -0.05,
      -0.03,
      -0.01,
      0.00,
      0.01,
      0.02,
      0.03,
      0.05,
      0.08
    ),
    inflation = c(
      0.010,
      0.015,
      0.020,
      0.025,
      0.030,
      0.032,
      0.035,
      0.038,
      0.040,
      0.045,
      0.050
    )
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        equity,
        target = 0.025
      )
    ) |>
    ffp_fit()
}


make_density_plot_data_pool <- function(
    confidence = 0.6
) {
  make_density_plot_data_fit() |>
    ffp_opinion_pooling(
      confidence = confidence
    )
}


# Fit ---------------------------------------------------------------------

test_that("density plot data supports ffp_fit", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity"
  )

  expect_identical(
    names(result),
    c(
      "density",
      "statistics",
      "bandwidth"
    )
  )

  expect_identical(
    levels(
      result$density$distribution
    ),
    c(
      "Prior",
      "Full confidence"
    )
  )

  expect_true(
    all(
      result$density$feature == "equity"
    )
  )
})


test_that("density plot data uses a common grid and bandwidth", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity",
    n = 200
  )

  prior <- result$density |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  full <- result$density |>
    dplyr::filter(
      .data$distribution == "Full confidence"
    )

  expect_equal(
    prior$x,
    full$x
  )

  expect_equal(
    prior$bandwidth,
    full$bandwidth
  )

  expect_equal(
    unique(prior$bandwidth),
    result$bandwidth
  )
})


# Opinion pooling ---------------------------------------------------------

test_that("density plot data adds posterior for opinion pooling", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity"
  )

  expect_identical(
    levels(
      result$density$distribution
    ),
    c(
      "Prior",
      "Full confidence",
      "Posterior"
    )
  )

  expect_identical(
    levels(
      result$statistics$distribution
    ),
    c(
      "Prior",
      "Full confidence",
      "Posterior"
    )
  )
})


test_that("posterior density remains the exact opinion-pool mixture", {
  confidence <- 0.35

  pooled <- make_density_plot_data_pool(
    confidence = confidence
  )

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity"
  )

  prior <- result$density |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  full <- result$density |>
    dplyr::filter(
      .data$distribution == "Full confidence"
    )

  posterior <- result$density |>
    dplyr::filter(
      .data$distribution == "Posterior"
    )

  expect_equal(
    posterior$density,
    (1 - confidence) * prior$density +
      confidence * full$density
  )
})


# Default statistics ------------------------------------------------------

test_that("density plot data includes informative default statistics", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity"
  )

  expect_identical(
    levels(
      result$statistics$statistic
    ),
    c(
      "Mean",
      "Median",
      "5% quantile"
    )
  )

  expect_equal(
    nrow(result$statistics),
    9L
  )
})


test_that("distribution means match ffp_stats()", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity"
  )

  values <- fit$model$scenarios$equity
  probabilities <- ffp_probabilities(fit)

  expected_prior <- ffp_stats(
    values,
    p = probabilities$prior,
    prob = 0.05
  )$statistics$mean[[1]]

  expected_full <- ffp_stats(
    values,
    p = probabilities$full_confidence,
    prob = 0.05
  )$statistics$mean[[1]]

  means <- result$statistics |>
    dplyr::filter(
      .data$statistic == "Mean"
    )

  expect_equal(
    means$value,
    c(
      expected_prior,
      expected_full
    )
  )
})


test_that("distribution medians match ffp_stats quantiles", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity"
  )

  values <- fit$model$scenarios$equity
  probabilities <- ffp_probabilities(fit)

  expected_prior <- ffp_stats(
    values,
    p = probabilities$prior,
    prob = 0.5
  )$statistics$quantile[[1]]

  expected_full <- ffp_stats(
    values,
    p = probabilities$full_confidence,
    prob = 0.5
  )$statistics$quantile[[1]]

  medians <- result$statistics |>
    dplyr::filter(
      .data$statistic == "Median"
    )

  expect_equal(
    medians$value,
    c(
      expected_prior,
      expected_full
    )
  )
})


test_that("distribution quantiles match ffp_stats()", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity",
    quantiles = 0.05
  )

  values <- fit$model$scenarios$equity
  probabilities <- ffp_probabilities(fit)

  expected_prior <- ffp_stats(
    values,
    p = probabilities$prior,
    prob = 0.05
  )$statistics$quantile[[1]]

  expected_full <- ffp_stats(
    values,
    p = probabilities$full_confidence,
    prob = 0.05
  )$statistics$quantile[[1]]

  quantiles <- result$statistics |>
    dplyr::filter(
      .data$statistic == "5% quantile"
    )

  expect_equal(
    quantiles$value,
    c(
      expected_prior,
      expected_full
    )
  )
})


test_that("statistic heights lie on their own weighted densities", {
  fit <- make_density_plot_data_fit()

  result <- prepare_distribution_density_plot_data(
    object = fit,
    feature = "equity"
  )

  values <- fit$model$scenarios$equity
  probabilities <- ffp_probabilities(fit)

  prior <- result$statistics |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  expected <- weighted_density_at(
    values = values,
    probabilities = probabilities$prior,
    x = prior$value,
    bandwidth = result$bandwidth
  )

  expect_equal(
    prior$density,
    expected
  )
})


# Custom statistics -------------------------------------------------------

test_that("statistics and quantiles are configurable", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity",
    statistics = "mean",
    quantiles = c(
      0.01,
      0.10
    )
  )

  expect_identical(
    levels(
      result$statistics$statistic
    ),
    c(
      "Mean",
      "1% quantile",
      "10% quantile"
    )
  )

  expect_equal(
    nrow(result$statistics),
    9L
  )
})


test_that("statistics can be omitted entirely", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity",
    statistics = NULL,
    quantiles = NULL
  )

  expect_s3_class(
    result$statistics,
    "tbl_df"
  )

  expect_equal(
    nrow(result$statistics),
    0L
  )

  expect_identical(
    names(result$statistics),
    c(
      "feature",
      "distribution",
      "statistic",
      "probability",
      "value",
      "density"
    )
  )
})


# Distribution selection -------------------------------------------------

test_that("distributions can be selected explicitly", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity",
    distributions = c(
      "prior",
      "posterior"
    )
  )

  expect_identical(
    levels(
      result$density$distribution
    ),
    c(
      "Prior",
      "Posterior"
    )
  )

  expect_identical(
    levels(
      result$statistics$distribution
    ),
    c(
      "Prior",
      "Posterior"
    )
  )

  expect_false(
    any(
      result$density$distribution ==
        "Full confidence"
    )
  )
})


test_that("distribution selection preserves requested order", {
  pooled <- make_density_plot_data_pool()

  result <- prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity",
    distributions = c(
      "posterior",
      "prior"
    )
  )

  expect_identical(
    levels(
      result$density$distribution
    ),
    c(
      "Posterior",
      "Prior"
    )
  )

  expect_identical(
    levels(
      result$statistics$distribution
    ),
    c(
      "Posterior",
      "Prior"
    )
  )
})


test_that("fit rejects posterior as an unavailable distribution", {
  fit <- make_density_plot_data_fit()

  expect_error(
    prepare_distribution_density_plot_data(
      object = fit,
      feature = "equity",
      distributions = "posterior"
    ),
    class = "ffp_error_invalid_plot_distributions"
  )
})


# Validation --------------------------------------------------------------

test_that("density plot statistics are validated", {
  fit <- make_density_plot_data_fit()

  invalid_statistics <- list(
    NA_character_,
    "",
    c(
      "mean",
      "mean"
    ),
    "sd",
    1
  )

  purrr::walk(
    invalid_statistics,
    \(statistics) {
      expect_error(
        prepare_distribution_density_plot_data(
          object = fit,
          feature = "equity",
          statistics = statistics
        ),
        class = "ffp_error_invalid_plot_statistics"
      )
    }
  )
})


test_that("density plot quantiles are validated", {
  fit <- make_density_plot_data_fit()

  invalid_quantiles <- list(
    0,
    1,
    -0.05,
    1.05,
    NA_real_,
    Inf,
    c(
      0.05,
      0.05
    ),
    "0.05"
  )

  purrr::walk(
    invalid_quantiles,
    \(quantiles) {
      expect_error(
        prepare_distribution_density_plot_data(
          object = fit,
          feature = "equity",
          quantiles = quantiles
        ),
        class = "ffp_error_invalid_plot_quantiles"
      )
    }
  )
})


test_that("density plot distributions are validated", {
  pooled <- make_density_plot_data_pool()

  invalid_distributions <- list(
    character(),
    NA_character_,
    "",
    c(
      "prior",
      "prior"
    ),
    "banana",
    1
  )

  purrr::walk(
    invalid_distributions,
    \(distributions) {
      expect_error(
        prepare_distribution_density_plot_data(
          object = pooled,
          feature = "equity",
          distributions = distributions
        ),
        class = "ffp_error_invalid_plot_distributions"
      )
    }
  )
})


test_that("density plot preparation preserves density validation", {
  fit <- make_density_plot_data_fit()

  expect_error(
    prepare_distribution_density_plot_data(
      object = fit,
      feature = "equity",
      adjust = 0
    ),
    class = "ffp_error_invalid_density_adjust"
  )
})


# Side effects ------------------------------------------------------------

test_that("density plot data does not modify the source object", {
  pooled <- make_density_plot_data_pool()

  original <- pooled

  prepare_distribution_density_plot_data(
    object = pooled,
    feature = "equity"
  )

  expect_identical(
    pooled,
    original
  )
})
