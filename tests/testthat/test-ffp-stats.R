test_that("ffp_stats computes equal-weight marginal statistics", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  stats <- ffp_stats(
    x = x,
    prob = 0.2
  )

  out <- tibble::as_tibble(stats)

  expect_s3_class(stats, "ffp_stats")

  expect_identical(
    out$variable,
    "x"
  )

  expect_equal(
    out$mean,
    0
  )

  expect_equal(
    out$sd,
    sqrt(2)
  )

  expect_equal(
    out$skewness,
    0
  )

  expect_equal(
    out$kurtosis,
    1.7
  )

  expect_equal(
    out$quantile,
    -2
  )

  expect_equal(
    out$expected_shortfall,
    -2
  )

  expect_equal(
    stats$n_scenarios,
    5L
  )

  expect_equal(
    stats$n_variables,
    1L
  )

  expect_equal(
    stats$effective_scenarios,
    5
  )

  expect_equal(
    stats$scenario_share,
    1
  )
})


test_that("ffp_stats uses flexible probabilities", {
  x <- c(
    -3,
    -1,
    2
  )

  p <- as_ffp(
    c(
      0.1,
      0.4,
      0.5
    )
  )

  stats <- ffp_stats(
    x = x,
    p = p,
    prob = 0.25
  )

  out <- tibble::as_tibble(stats)

  expect_equal(
    out$quantile,
    -1
  )

  expect_equal(
    out$expected_shortfall,
    -1.8
  )
})


test_that("expected shortfall uses exactly the requested tail mass", {
  x <- c(
    -10,
    -2,
    1,
    3
  )

  p <- as_ffp(
    c(
      0.05,
      0.35,
      0.30,
      0.30
    )
  )

  stats <- ffp_stats(
    x = x,
    p = p,
    prob = 0.20
  )

  out <- tibble::as_tibble(stats)

  expected <- (
    -10 * 0.05 +
      -2 * 0.15
  ) / 0.20

  expect_equal(
    out$quantile,
    -2
  )

  expect_equal(
    out$expected_shortfall,
    expected
  )
})


test_that("ffp_stats preserves variable names", {
  x <- matrix(
    c(
      1, 4,
      2, 5,
      3, 6
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  stats <- ffp_stats(x)

  expect_identical(
    stats$statistics$variable,
    c("asset_a", "asset_b")
  )
})


test_that("ffp_stats uses numeric columns from data frames", {
  x <- data.frame(
    date = as.Date("2026-01-01") + 0:2,
    asset_a = c(1, 2, 3),
    asset_b = c(4, 5, 6)
  )

  stats <- ffp_stats(x)

  expect_identical(
    stats$statistics$variable,
    c("asset_a", "asset_b")
  )

  expect_equal(
    stats$n_variables,
    2L
  )
})


test_that("ffp_stats handles degenerate distributions", {
  stats <- ffp_stats(
    x = rep(2, 4),
    prob = 0.25
  )

  out <- tibble::as_tibble(stats)

  expect_equal(
    out$mean,
    2
  )

  expect_equal(
    out$sd,
    0
  )

  expect_true(
    is.na(out$skewness)
  )

  expect_true(
    is.na(out$kurtosis)
  )

  expect_equal(
    out$quantile,
    2
  )

  expect_equal(
    out$expected_shortfall,
    2
  )
})


test_that("ffp_stats validates tail probability", {
  expect_error(
    ffp_stats(
      1:5,
      prob = 0
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    ffp_stats(
      1:5,
      prob = 1
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    ffp_stats(
      1:5,
      prob = c(0.01, 0.05)
    ),
    "single finite number"
  )

  expect_error(
    ffp_stats(
      1:5,
      prob = NA_real_
    ),
    "single finite number"
  )
})


test_that("ffp_stats validates scenario and probability sizes", {
  expect_error(
    ffp_stats(
      x = 1:3,
      p = c(0.5, 0.5)
    ),
    "same number of observations"
  )
})


test_that("ffp_stats rejects non-finite scenarios", {
  expect_error(
    ffp_stats(
      c(
        1,
        NA_real_,
        3
      )
    ),
    "finite values"
  )

  expect_error(
    ffp_stats(
      c(
        1,
        Inf,
        3
      )
    ),
    "finite values"
  )
})


test_that("ffp_stats print returns invisibly", {
  stats <- ffp_stats(1:4)

  expect_invisible(
    print(stats)
  )
})

test_that("ffp_stats comparison stores prior posterior and change", {
  x <- matrix(
    c(
      -2, 1,
      -1, 2,
      0, 3,
      1, 4,
      2, 5
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  prior <- as_ffp(
    rep(
      1 / nrow(x),
      nrow(x)
    )
  )

  posterior <- as_ffp(
    c(
      0.05,
      0.10,
      0.15,
      0.20,
      0.50
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = prior,
    posterior = posterior,
    prob = 0.20
  )

  expect_s3_class(
    stats,
    "ffp_stats"
  )

  expect_identical(
    stats$type,
    "comparison"
  )

  expect_identical(
    stats$prior$variable,
    c("asset_a", "asset_b")
  )

  expect_identical(
    stats$posterior$variable,
    c("asset_a", "asset_b")
  )

  expect_identical(
    stats$change$variable,
    c("asset_a", "asset_b")
  )
})


test_that("ffp_stats comparison computes changes as posterior minus prior", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  prior <- as_ffp(
    rep(
      0.2,
      5
    )
  )

  posterior <- as_ffp(
    c(
      0.05,
      0.10,
      0.15,
      0.20,
      0.50
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = prior,
    posterior = posterior,
    prob = 0.20
  )

  statistic_names <- setdiff(
    names(stats$prior),
    "variable"
  )

  expect_s3_class(
    stats$change,
    "tbl_df"
  )

  expect_equal(
    as.matrix(
      stats$change[statistic_names]
    ),
    as.matrix(
      stats$posterior[statistic_names]
    ) -
      as.matrix(
        stats$prior[statistic_names]
      )
  )
})


test_that("ffp_stats comparison reports effective scenario diagnostics", {
  x <- 1:4

  prior <- as_ffp(
    rep(
      0.25,
      4
    )
  )

  posterior <- as_ffp(
    c(
      0.5,
      0.5,
      0,
      0
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = prior,
    posterior = posterior
  )

  expect_equal(
    stats$effective_scenarios$ens,
    c(
      4,
      2
    )
  )

  expect_equal(
    stats$effective_scenarios$scenario_share,
    c(
      1,
      0.5
    )
  )

  expect_equal(
    stats$ens_retained,
    0.5
  )
})


test_that("as_tibble.ffp_stats returns prior and posterior distributions", {
  x <- 1:4

  prior <- as_ffp(
    rep(
      0.25,
      4
    )
  )

  posterior <- as_ffp(
    c(
      0.1,
      0.2,
      0.3,
      0.4
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = prior,
    posterior = posterior
  )

  out <- tibble::as_tibble(stats)

  expect_identical(
    out$distribution,
    c(
      "prior",
      "posterior"
    )
  )

  expect_identical(
    out$variable,
    c(
      "x",
      "x"
    )
  )
})


test_that("ffp_stats comparison print returns invisibly", {
  x <- 1:4

  stats <- ffp_stats_compare(
    x = x,
    prior = as_ffp(
      rep(
        0.25,
        4
      )
    ),
    posterior = as_ffp(
      c(
        0.5,
        0.5,
        0,
        0
      )
    )
  )

  expect_invisible(
    print(stats)
  )
})

test_that("ffp_stats default method preserves the existing interface", {
  stats <- ffp_stats(
    1:4,
    p = as_ffp(
      rep(
        0.25,
        4
      )
    )
  )

  expect_s3_class(
    stats,
    "ffp_stats"
  )

  expect_identical(
    stats$type,
    "single"
  )

  expect_equal(
    stats$effective_scenarios,
    4
  )
})

test_that("ffp_stats comparison computes changes as posterior minus prior", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  prior <- as_ffp(
    rep(
      0.2,
      5
    )
  )

  posterior <- as_ffp(
    c(
      0.05,
      0.10,
      0.15,
      0.20,
      0.50
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = prior,
    posterior = posterior,
    prob = 0.20
  )

  statistic_names <- setdiff(
    names(stats$prior),
    "variable"
  )

  expect_equal(
    as.matrix(
      stats$change[statistic_names]
    ),
    as.matrix(
      stats$posterior[statistic_names]
    ) -
      as.matrix(
        stats$prior[statistic_names]
      )
  )
})

test_that("summary.ffp_stats summarizes one variable", {
  x <- matrix(
    c(
      -2, 1,
      -1, 2,
      0, 3,
      1, 4,
      2, 5
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  stats <- ffp_stats(x)

  out <- summary(
    stats,
    variable = "asset_a"
  )

  expect_s3_class(
    out,
    "summary_ffp_stats"
  )

  expect_identical(
    out$view,
    "variable"
  )

  expect_identical(
    out$label,
    "asset_a"
  )

  expect_identical(
    out$data$statistic,
    ffp_stats_statistic_names()
  )

  expect_identical(
    names(out$data),
    c(
      "statistic",
      "value"
    )
  )
})


test_that("summary.ffp_stats compares prior and posterior by variable", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = as_ffp(
      rep(
        0.2,
        5
      )
    ),
    posterior = as_ffp(
      c(
        0.05,
        0.10,
        0.15,
        0.20,
        0.50
      )
    )
  )

  out <- summary(
    stats,
    variable = "x"
  )

  expect_identical(
    names(out$data),
    c(
      "statistic",
      "prior",
      "posterior",
      "change"
    )
  )

  expect_equal(
    out$data$change,
    out$data$posterior -
      out$data$prior
  )
})


test_that("summary.ffp_stats summarizes one statistic across variables", {
  x <- matrix(
    c(
      -2, 1,
      -1, 2,
      0, 3,
      1, 4,
      2, 5
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  stats <- ffp_stats(x)

  out <- summary(
    stats,
    statistic = "sd"
  )

  expect_identical(
    out$view,
    "statistic"
  )

  expect_identical(
    out$label,
    "sd"
  )

  expect_identical(
    out$data$variable,
    c(
      "asset_a",
      "asset_b"
    )
  )

  expect_identical(
    names(out$data),
    c(
      "variable",
      "value"
    )
  )
})


test_that("summary.ffp_stats compares one statistic across variables", {
  x <- matrix(
    c(
      -2, 1,
      -1, 2,
      0, 3,
      1, 4,
      2, 5
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  stats <- ffp_stats_compare(
    x = x,
    prior = as_ffp(
      rep(
        0.2,
        5
      )
    ),
    posterior = as_ffp(
      c(
        0.05,
        0.10,
        0.15,
        0.20,
        0.50
      )
    )
  )

  out <- summary(
    stats,
    statistic = "mean"
  )

  expect_identical(
    names(out$data),
    c(
      "variable",
      "prior",
      "posterior",
      "change"
    )
  )

  expect_equal(
    out$data$change,
    out$data$posterior -
      out$data$prior
  )
})


test_that("summary.ffp_stats validates selectors", {
  stats <- ffp_stats(
    matrix(
      1:6,
      ncol = 2,
      dimnames = list(
        NULL,
        c("asset_a", "asset_b")
      )
    )
  )

  expect_error(
    summary(stats),
    "Supply either"
  )

  expect_error(
    summary(
      stats,
      variable = "asset_a",
      statistic = "mean"
    ),
    "Supply only one"
  )

  expect_error(
    summary(
      stats,
      variable = "unknown"
    ),
    "Unknown variable"
  )

  expect_error(
    summary(
      stats,
      statistic = "variance"
    ),
    "Unknown statistic"
  )
})


test_that("as_tibble.summary_ffp_stats exposes summary data", {
  stats <- ffp_stats(
    matrix(
      1:6,
      ncol = 2,
      dimnames = list(
        NULL,
        c("asset_a", "asset_b")
      )
    )
  )

  summary_stats <- summary(
    stats,
    statistic = "mean"
  )

  expect_identical(
    tibble::as_tibble(summary_stats),
    summary_stats$data
  )
})


test_that("summary.ffp_stats print returns invisibly", {
  stats <- ffp_stats(1:5)

  out <- summary(
    stats,
    variable = "x"
  )

  expect_invisible(
    print(out)
  )
})

test_that("ffp_stats works with an ffp_fit object", {
  scenarios <- data.frame(
    asset_a = c(
      -0.02,
      -0.01,
      0.01,
      0.02
    ),
    asset_b = c(
      0.01,
      0.00,
      -0.01,
      0.02
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  fit <- ffp_fit(model)

  stats <- ffp_stats(fit)

  expect_s3_class(
    stats,
    "ffp_stats"
  )

  expect_identical(
    stats$type,
    "comparison"
  )

  expect_identical(
    stats$prior$variable,
    c(
      "asset_a",
      "asset_b"
    )
  )

  expect_identical(
    stats$posterior$variable,
    c(
      "asset_a",
      "asset_b"
    )
  )
})

test_that("ffp_stats fit comparison is unchanged when prior equals posterior", {
  scenarios <- data.frame(
    asset_a = c(
      -2,
      -1,
      1,
      2
    ),
    asset_b = c(
      1,
      2,
      3,
      4
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  fit <- ffp_fit(model)

  stats <- ffp_stats(
    fit,
    prob = 0.25
  )

  statistic_names <- ffp_stats_statistic_names()

  expect_equal(
    as.matrix(
      stats$prior[statistic_names]
    ),
    as.matrix(
      stats$posterior[statistic_names]
    )
  )

  expect_equal(
    unname(
      as.matrix(
        stats$change[statistic_names]
      )
    ),
    matrix(
      0,
      nrow = nrow(stats$change),
      ncol = length(statistic_names)
    )
  )

  expect_equal(
    stats$ens_retained,
    1
  )
})

test_that("ffp_stats reflects distributional changes caused by views", {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        equity,
        target = 0.02
      )
    )

  fit <- ffp_fit(model)

  stats <- ffp_stats(fit)

  prior_mean <- stats$prior$mean
  posterior_mean <- stats$posterior$mean

  expect_equal(
    posterior_mean,
    0.02,
    tolerance = 1e-8
  )

  expect_equal(
    stats$change$mean,
    posterior_mean - prior_mean
  )

  expect_lte(
    stats$effective_scenarios$ens[
      stats$effective_scenarios$distribution == "posterior"
    ],
    stats$effective_scenarios$ens[
      stats$effective_scenarios$distribution == "prior"
    ]
  )
})

test_that("ffp_stats summaries work directly from fitted models", {
  scenarios <- data.frame(
    asset_a = c(
      -0.02,
      -0.01,
      0.01,
      0.02
    ),
    asset_b = c(
      0.01,
      0.00,
      -0.01,
      0.02
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  fit <- ffp_fit(model)

  stats <- ffp_stats(fit)

  by_variable <- summary(
    stats,
    variable = "asset_a"
  )

  by_statistic <- summary(
    stats,
    statistic = "mean"
  )

  expect_s3_class(
    by_variable,
    "summary_ffp_stats"
  )

  expect_s3_class(
    by_statistic,
    "summary_ffp_stats"
  )

  expect_identical(
    by_variable$label,
    "asset_a"
  )

  expect_identical(
    by_statistic$label,
    "mean"
  )
})
