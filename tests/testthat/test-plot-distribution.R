# Helpers -----------------------------------------------------------------

make_plot_distribution_fit <- function() {
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


make_plot_distribution_pool <- function(
    confidence = 0.6
) {
  make_plot_distribution_fit() |>
    ffp_opinion_pooling(
      confidence = confidence
    )
}


# Public API ---------------------------------------------------------------

test_that("ffp_plot_distribution() returns a ggplot", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_s3_class(
    plot,
    "ggplot"
  )
})


test_that("ffp_plot_distribution() has a focused public interface", {
  expect_identical(
    names(
      formals(ffp_plot_distribution)
    ),
    c(
      "object",
      "feature",
      "distributions",
      "statistics",
      "quantiles",
      "adjust",
      "..."
    )
  )
})


test_that("ffp_plot_distribution() requires a valid feature", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(fit),
    class = "ffp_error_invalid_plot_feature"
  )
})


test_that("ffp_plot_distribution() rejects unused arguments", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "equity",
      banana = TRUE
    )
  )
})


# Fit ---------------------------------------------------------------------

test_that("fit plot contains prior and full confidence", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_identical(
    levels(
      plot$data$distribution
    ),
    c(
      "Prior",
      "Full confidence"
    )
  )
})


test_that("fit plot contains density lines and statistic segments", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_length(
    plot$layers,
    2L
  )

  expect_s3_class(
    plot$layers[[1]]$geom,
    "GeomLine"
  )

  expect_s3_class(
    plot$layers[[2]]$geom,
    "GeomSegment"
  )
})


test_that("fit plot uses distribution for density colour", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_equal(
    rlang::as_label(
      plot$mapping$colour
    ),
    "distribution"
  )

  expect_equal(
    rlang::as_label(
      plot$mapping$group
    ),
    "distribution"
  )
})


test_that("statistic segments map distribution and statistic", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  segment_mapping <- plot$layers[[2]]$mapping

  expect_equal(
    rlang::as_label(
      segment_mapping$colour
    ),
    "distribution"
  )

  expect_equal(
    rlang::as_label(
      segment_mapping$linetype
    ),
    "statistic"
  )

  expect_equal(
    rlang::as_label(
      segment_mapping$x
    ),
    "value"
  )

  expect_equal(
    rlang::as_label(
      segment_mapping$xend
    ),
    "value"
  )

  expect_equal(
    rlang::as_label(
      segment_mapping$yend
    ),
    "density"
  )
})


test_that("statistic segments start at zero", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  segment_mapping <- plot$layers[[2]]$mapping

  expect_equal(
    rlang::eval_tidy(
      segment_mapping$y
    ),
    0
  )
})


test_that("default statistic labels are informative", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  statistics <- plot$layers[[2]]$data

  expect_identical(
    levels(
      statistics$statistic
    ),
    c(
      "Mean",
      "Median",
      "5% quantile"
    )
  )
})


# Opinion pooling ---------------------------------------------------------

test_that("opinion pool plot contains all three distributions", {
  pooled <- make_plot_distribution_pool()

  plot <- ffp_plot_distribution(
    pooled,
    feature = "equity"
  )

  expect_identical(
    levels(
      plot$data$distribution
    ),
    c(
      "Prior",
      "Full confidence",
      "Posterior"
    )
  )
})


test_that("opinion pool statistics cover all three distributions", {
  pooled <- make_plot_distribution_pool()

  plot <- ffp_plot_distribution(
    pooled,
    feature = "equity"
  )

  statistics <- plot$layers[[2]]$data

  expect_identical(
    levels(
      statistics$distribution
    ),
    c(
      "Prior",
      "Full confidence",
      "Posterior"
    )
  )

  expect_equal(
    nrow(statistics),
    9L
  )
})


# Customization -----------------------------------------------------------

test_that("distributions can be selected", {
  pooled <- make_plot_distribution_pool()

  plot <- ffp_plot_distribution(
    pooled,
    feature = "equity",
    distributions = c(
      "prior",
      "posterior"
    )
  )

  expect_identical(
    levels(
      plot$data$distribution
    ),
    c(
      "Prior",
      "Posterior"
    )
  )

  expect_equal(
    nrow(
      plot$layers[[2]]$data
    ),
    6L
  )
})


test_that("statistics and quantiles can be customized", {
  pooled <- make_plot_distribution_pool()

  plot <- ffp_plot_distribution(
    pooled,
    feature = "equity",
    statistics = "mean",
    quantiles = c(
      0.01,
      0.10
    )
  )

  statistics <- plot$layers[[2]]$data

  expect_identical(
    levels(
      statistics$statistic
    ),
    c(
      "Mean",
      "1% quantile",
      "10% quantile"
    )
  )
})


test_that("statistic annotations can be omitted", {
  pooled <- make_plot_distribution_pool()

  plot <- ffp_plot_distribution(
    pooled,
    feature = "equity",
    statistics = NULL,
    quantiles = NULL
  )

  expect_length(
    plot$layers,
    1L
  )

  expect_s3_class(
    plot$layers[[1]]$geom,
    "GeomLine"
  )
})


test_that("adjust changes density smoothing", {
  fit <- make_plot_distribution_fit()

  baseline <- ffp_plot_distribution(
    fit,
    feature = "equity",
    adjust = 1
  )

  smoother <- ffp_plot_distribution(
    fit,
    feature = "equity",
    adjust = 1.5
  )

  expect_false(
    isTRUE(
      all.equal(
        baseline$data$density,
        smoother$data$density
      )
    )
  )
})


# Labels ------------------------------------------------------------------

test_that("distribution plot uses public-facing labels", {
  fit <- make_plot_distribution_fit()

  plot <- ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_equal(
    plot$labels$x,
    "equity"
  )

  expect_equal(
    plot$labels$y,
    "Density"
  )

  expect_equal(
    plot$labels$colour,
    "Distribution"
  )

  expect_equal(
    plot$labels$linetype,
    "Statistic"
  )
})


# Validation --------------------------------------------------------------

test_that("distribution plot preserves feature validation", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "unknown"
    ),
    class = "ffp_error_unknown_plot_feature"
  )
})


test_that("distribution plot preserves distribution validation", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "equity",
      distributions = "posterior"
    ),
    class = "ffp_error_invalid_plot_distributions"
  )
})


test_that("distribution plot preserves statistic validation", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "equity",
      statistics = "sd"
    ),
    class = "ffp_error_invalid_plot_statistics"
  )
})


test_that("distribution plot preserves quantile validation", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "equity",
      quantiles = 1
    ),
    class = "ffp_error_invalid_plot_quantiles"
  )
})


test_that("distribution plot preserves adjust validation", {
  fit <- make_plot_distribution_fit()

  expect_error(
    ffp_plot_distribution(
      fit,
      feature = "equity",
      adjust = 0
    ),
    class = "ffp_error_invalid_density_adjust"
  )
})


# Side effects ------------------------------------------------------------

test_that("distribution plot does not modify the source object", {
  pooled <- make_plot_distribution_pool()

  original <- pooled

  ffp_plot_distribution(
    pooled,
    feature = "equity"
  )

  expect_identical(
    pooled,
    original
  )
})


test_that("distribution plot does not modify the global theme", {
  fit <- make_plot_distribution_fit()

  original_theme <- ggplot2::theme_get()

  ffp_plot_distribution(
    fit,
    feature = "equity"
  )

  expect_identical(
    ggplot2::theme_get(),
    original_theme
  )
})
