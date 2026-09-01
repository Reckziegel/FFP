# Helpers -----------------------------------------------------------------

make_autoplot_fit <- function() {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.05,
      -0.03,
      -0.01,
      0.00,
      0.01,
      0.02,
      0.03,
      0.04,
      0.06,
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


# Public API --------------------------------------------------------------

test_that("autoplot.ffp_fit() returns a ggplot object", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_true(
    inherits(
      plot,
      "ggplot"
    )
  )
})


test_that("autoplot.ffp_fit() exposes the intended public API", {
  expect_identical(
    names(
      formals(
        autoplot.ffp_fit
      )
    ),
    c(
      "object",
      "feature",
      "bins",
      "..."
    )
  )
})


test_that("autoplot.ffp_fit() requires an explicit feature", {
  fit <- make_autoplot_fit()

  expect_error(
    ggplot2::autoplot(fit),
    class = "ffp_error_invalid_plot_feature"
  )
})


test_that("autoplot.ffp_fit() rejects unused arguments", {
  fit <- make_autoplot_fit()

  expect_error(
    ggplot2::autoplot(
      fit,
      feature = "equity",
      unknown = TRUE
    )
  )
})


# Histogram semantics -----------------------------------------------------

test_that("autoplot uses prior and full-confidence distributions", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 5
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


test_that("autoplot uses one common path per distribution", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 5
  )

  distributions <- unique(
    as.character(
      plot$data$distribution
    )
  )

  expect_setequal(
    distributions,
    c(
      "Prior",
      "Full confidence"
    )
  )
})


test_that("histogram paths start and finish at zero density", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 5
  )

  endpoints <- plot$data |>
    dplyr::group_by(
      .data$distribution
    ) |>
    dplyr::summarise(
      first_density = dplyr::first(
        .data$density
      ),
      last_density = dplyr::last(
        .data$density
      ),
      .groups = "drop"
    )

  expect_equal(
    endpoints$first_density,
    c(
      0,
      0
    )
  )

  expect_equal(
    endpoints$last_density,
    c(
      0,
      0
    )
  )
})


test_that("histogram paths use exact bin boundaries", {
  fit <- make_autoplot_fit()

  histogram <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 5
  )

  path <- distribution_histogram_path(
    histogram
  )

  prior_histogram <- histogram |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  prior_path <- path |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  expected_x <- c(
    prior_histogram$lower[[1]],
    as.vector(
      rbind(
        prior_histogram$lower,
        prior_histogram$upper
      )
    ),
    prior_histogram$upper[[
      nrow(prior_histogram)
    ]]
  )

  expect_equal(
    prior_path$x,
    expected_x
  )
})


test_that("histogram paths preserve bin densities", {
  fit <- make_autoplot_fit()

  histogram <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 5
  )

  path <- distribution_histogram_path(
    histogram
  )

  prior_histogram <- histogram |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  prior_path <- path |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  expected_density <- c(
    0,
    rep(
      prior_histogram$density,
      each = 2L
    ),
    0
  )

  expect_equal(
    prior_path$density,
    expected_density
  )
})


test_that("autoplot densities correspond to histogram data", {
  fit <- make_autoplot_fit()

  histogram <- prepare_distribution_plot_data(
    object = fit,
    feature = "equity",
    bins = 5
  )

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 5
  )

  prior_histogram <- histogram |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  prior_path <- plot$data |>
    dplyr::filter(
      .data$distribution == "Prior"
    )

  plotted_bin_densities <- prior_path$density[
    seq(
      from = 2L,
      to = nrow(prior_path) - 1L,
      by = 2L
    )
  ]

  expect_equal(
    plotted_bin_densities,
    prior_histogram$density
  )
})


# Geometry ----------------------------------------------------------------

test_that("autoplot uses a path geometry", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_length(
    plot$layers,
    1L
  )

  expect_true(
    inherits(
      plot$layers[[1]]$geom,
      "GeomPath"
    )
  )
})


test_that("autoplot maps distribution to colour and group", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_true(
    "colour" %in%
      names(plot$mapping)
  )

  expect_true(
    "group" %in%
      names(plot$mapping)
  )
})


# Labels ------------------------------------------------------------------

test_that("autoplot uses public distribution labels", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
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

  expect_identical(
    plot$labels$colour,
    "Distribution"
  )
})


test_that("autoplot labels axes from the selected feature", {
  fit <- make_autoplot_fit()

  plot <- ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_identical(
    plot$labels$x,
    "equity"
  )

  expect_identical(
    plot$labels$y,
    "Density"
  )
})


test_that("autoplot changes the x label with the selected feature", {
  fit <- make_autoplot_fit()

  equity_plot <- ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  inflation_plot <- ggplot2::autoplot(
    fit,
    feature = "inflation"
  )

  expect_identical(
    equity_plot$labels$x,
    "equity"
  )

  expect_identical(
    inflation_plot$labels$x,
    "inflation"
  )
})


# Bins --------------------------------------------------------------------

test_that("autoplot respects an explicit bin count", {
  fit <- make_autoplot_fit()

  few_bins <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 2
  )

  many_bins <- ggplot2::autoplot(
    fit,
    feature = "equity",
    bins = 8
  )

  expect_gt(
    nrow(many_bins$data),
    nrow(few_bins$data)
  )
})


test_that("autoplot preserves histogram bin validation", {
  fit <- make_autoplot_fit()

  expect_error(
    ggplot2::autoplot(
      fit,
      feature = "equity",
      bins = 0
    ),
    class = "ffp_error_invalid_histogram_bins"
  )

  expect_error(
    ggplot2::autoplot(
      fit,
      feature = "equity",
      bins = 2.5
    ),
    class = "ffp_error_invalid_histogram_bins"
  )
})


# Feature validation ------------------------------------------------------

test_that("autoplot rejects unknown features", {
  fit <- make_autoplot_fit()

  expect_error(
    ggplot2::autoplot(
      fit,
      feature = "rates"
    ),
    class = "ffp_error_unknown_plot_feature"
  )
})


test_that("autoplot rejects invalid feature specifications", {
  fit <- make_autoplot_fit()

  invalid_features <- list(
    character(),
    c(
      "equity",
      "inflation"
    ),
    NA_character_,
    "",
    1
  )

  purrr::walk(
    invalid_features,
    \(feature) {
      expect_error(
        ggplot2::autoplot(
          fit,
          feature = feature
        ),
        class = "ffp_error_invalid_plot_feature"
      )
    }
  )
})


# Side effects ------------------------------------------------------------

test_that("autoplot does not modify the fitted object", {
  fit <- make_autoplot_fit()
  original <- fit

  ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_identical(
    fit,
    original
  )
})


test_that("autoplot does not modify the global ggplot2 theme", {
  fit <- make_autoplot_fit()

  theme_before <- ggplot2::theme_get()

  ggplot2::autoplot(
    fit,
    feature = "equity"
  )

  expect_identical(
    ggplot2::theme_get(),
    theme_before
  )
})

# # Helpers -----------------------------------------------------------------
#
# make_autoplot_test_fit <- function(index = NULL) {
#   scenarios <- matrix(
#     c(
#       -0.08,
#       -0.03,
#       0.01,
#       0.04,
#       0.06
#     ),
#     ncol = 1,
#     dimnames = list(
#       index,
#       "equity"
#     )
#   )
#
#   ffp_model(scenarios) |>
#     ffp_prior(
#       prior_uniform()
#     ) |>
#     ffp_view(
#       view_mean(
#         equity,
#         target = 0.02
#       )
#     ) |>
#     ffp_fit()
# }
#
#
# # Public API --------------------------------------------------------------
#
# test_that("autoplot.ffp_fit() returns a ggplot object", {
#   fit <- make_autoplot_test_fit()
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_s3_class(plot, "ggplot")
# })
#
#
# test_that("autoplot.ffp_fit() exposes the intended public API", {
#   expect_identical(
#     names(formals(autoplot.ffp_fit)),
#     c(
#       "object",
#       "x",
#       "..."
#     )
#   )
# })
#
#
# test_that("autoplot.ffp_fit() validates x", {
#   fit <- make_autoplot_test_fit()
#
#   expect_error(
#     ggplot2::autoplot(
#       fit,
#       x = "time"
#     ),
#     class = "ffp_error_invalid_plot_x"
#   )
#
#   expect_error(
#     ggplot2::autoplot(
#       fit,
#       x = c("index", "scenario")
#     ),
#     class = "ffp_error_invalid_plot_x"
#   )
#
#   expect_error(
#     ggplot2::autoplot(
#       fit,
#       x = NA_character_
#     ),
#     class = "ffp_error_invalid_plot_x"
#   )
# })
#
#
# test_that("autoplot.ffp_fit() rejects unused arguments", {
#   fit <- make_autoplot_test_fit()
#
#   expect_error(
#     ggplot2::autoplot(
#       fit,
#       unknown = TRUE
#     )
#   )
# })
#
#
# # Scenario axis -----------------------------------------------------------
#
# test_that("autoplot uses scenario numbers when no index exists", {
#   fit <- make_autoplot_test_fit()
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_identical(
#     plot$labels$x,
#     "Scenario"
#   )
#
#   expect_identical(
#     sort(unique(plot$data$.ffp_x)),
#     seq_len(fit$n_scenarios)
#   )
# })
#
#
# test_that("explicit scenario axis ignores an available index", {
#   index <- as.character(
#     as.Date("2026-01-01") + 0:4
#   )
#
#   fit <- make_autoplot_test_fit(index)
#
#   plot <- ggplot2::autoplot(
#     fit,
#     x = "scenario"
#   )
#
#   expect_identical(
#     plot$labels$x,
#     "Scenario"
#   )
#
#   expect_identical(
#     sort(unique(plot$data$.ffp_x)),
#     seq_len(fit$n_scenarios)
#   )
# })
#
#
# test_that("scenario plots use segments and points", {
#   fit <- make_autoplot_test_fit()
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_length(plot$layers, 2L)
#
#   expect_s3_class(
#     plot$layers[[1]]$geom,
#     "GeomSegment"
#   )
#
#   expect_s3_class(
#     plot$layers[[2]]$geom,
#     "GeomPoint"
#   )
# })
#
#
# # Index axis --------------------------------------------------------------
#
# test_that("autoplot uses an available non-temporal index", {
#   index <- paste0(
#     "state_",
#     seq_len(5)
#   )
#
#   fit <- make_autoplot_test_fit(index)
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_identical(
#     plot$labels$x,
#     "Index"
#   )
#
#   expect_setequal(
#     unique(plot$data$.ffp_x),
#     index
#   )
#
#   expect_length(plot$layers, 2L)
#
#   expect_s3_class(
#     plot$layers[[1]]$geom,
#     "GeomSegment"
#   )
#
#   expect_s3_class(
#     plot$layers[[2]]$geom,
#     "GeomPoint"
#   )
# })
#
#
# test_that("autoplot uses lines for temporal indexes", {
#   index <- as.character(
#     as.Date("2026-01-01") + 0:4
#   )
#
#   fit <- make_autoplot_test_fit(index)
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_identical(
#     plot$labels$x,
#     "Index"
#   )
#
#   expect_s3_class(
#     plot$data$.ffp_x,
#     "Date"
#   )
#
#   expect_length(plot$layers, 1L)
#
#   expect_s3_class(
#     plot$layers[[1]]$geom,
#     "GeomLine"
#   )
# })
#
#
# test_that("x = index requires an actual scenario index", {
#   fit <- make_autoplot_test_fit()
#
#   expect_error(
#     ggplot2::autoplot(
#       fit,
#       x = "index"
#     ),
#     class = "ffp_error_missing_plot_index"
#   )
# })
#
#
# # Probability semantics ---------------------------------------------------
#
# test_that("autoplot compares prior and full-confidence probabilities", {
#   fit <- make_autoplot_test_fit()
#
#   probabilities <- ffp_probabilities(fit)
#   plot <- ggplot2::autoplot(fit)
#
#   expect_identical(
#     levels(plot$data$distribution),
#     c(
#       "Prior",
#       "Full confidence"
#     )
#   )
#
#   plotted_prior <- plot$data |>
#     dplyr::filter(
#       .data$distribution == "Prior"
#     ) |>
#     dplyr::pull(
#       .data$probability
#     )
#
#   plotted_full <- plot$data |>
#     dplyr::filter(
#       .data$distribution == "Full confidence"
#     ) |>
#     dplyr::pull(
#       .data$probability
#     )
#
#   expect_equal(
#     plotted_prior,
#     probabilities$prior
#   )
#
#   expect_equal(
#     plotted_full,
#     probabilities$full_confidence
#   )
# })
#
#
# test_that("autoplot uses public probability labels", {
#   fit <- make_autoplot_test_fit()
#
#   plot <- ggplot2::autoplot(fit)
#
#   expect_identical(
#     plot$labels$y,
#     "Probability"
#   )
#
#   expect_identical(
#     plot$labels$colour,
#     "Distribution"
#   )
# })
#
#
# # Side effects ------------------------------------------------------------
#
# test_that("autoplot does not modify the fitted object", {
#   fit <- make_autoplot_test_fit()
#   original <- fit
#
#   ggplot2::autoplot(fit)
#
#   expect_identical(
#     fit,
#     original
#   )
# })
#
#
# test_that("autoplot does not modify the global ggplot2 theme", {
#   fit <- make_autoplot_test_fit()
#   theme_before <- ggplot2::theme_get()
#
#   ggplot2::autoplot(fit)
#
#   expect_identical(
#     ggplot2::theme_get(),
#     theme_before
#   )
# })
