test_that("ffp_moments preserves legacy weighted-moment semantics", {
  x <- matrix(
    c(
      -2, 1,
      -1, 3,
      0, -1,
      1, 2,
      3, 4
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("asset_a", "asset_b")
    )
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.30
  )

  expected <- stats::cov.wt(
    x = x,
    wt = probabilities,
    method = "ML"
  )

  result <- ffp_moments(
    x = x,
    p = probabilities
  )

  expect_equal(
    result$mu,
    expected$center,
    tolerance = 1e-14
  )

  expect_equal(
    result$sigma,
    expected$cov,
    tolerance = 1e-14
  )
})

test_that("ffp_moments uses the fitted posterior for ffp_fit objects", {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    ),
    bonds = c(
      0.03,
      0.02,
      0.01,
      0.00,
      -0.01
    )
  )

  fit <- ffp_model(scenarios) |>
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

  result <- ffp_moments(
    fit
  )

  expected <- ffp_moments(
    x = fit$model$scenarios,
    p = fit$posterior
  )

  expect_equal(
    result$mu,
    expected$mu
  )

  expect_equal(
    result$sigma,
    expected$sigma
  )

  expect_equal(
    unname(result$mu[["equity"]]),
    0.02,
    tolerance = 1e-8
  )
})

test_that("ffp_moments uses the effective opinion-pooled posterior", {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    ),
    bonds = c(
      0.03,
      0.02,
      0.01,
      0.00,
      -0.01
    )
  )

  fit <- ffp_model(scenarios) |>
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

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  result <- ffp_moments(
    pooled
  )

  expected <- ffp_moments(
    x = pooled$fit$model$scenarios,
    p = pooled$posterior
  )

  full_confidence <- ffp_moments(
    fit
  )

  expect_equal(
    result$mu,
    expected$mu
  )

  expect_equal(
    result$sigma,
    expected$sigma
  )

  expect_false(
    isTRUE(
      all.equal(
        result$mu,
        full_confidence$mu
      )
    )
  )
})

test_that("opinion-pooled means interpolate prior and full-confidence means", {
  scenarios <- data.frame(
    equity = c(
      -0.08,
      -0.03,
      0.01,
      0.04,
      0.06
    )
  )

  fit <- ffp_model(scenarios) |>
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

  confidence <- 0.40

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = confidence
  )

  prior_moments <- ffp_moments(
    x = fit$model$scenarios,
    p = fit$prior
  )

  full_moments <- ffp_moments(
    fit
  )

  pooled_moments <- ffp_moments(
    pooled
  )

  expected_mean <- (
    (1 - confidence) * prior_moments$mu +
      confidence * full_moments$mu
  )

  expect_equal(
    pooled_moments$mu,
    expected_mean,
    tolerance = 1e-14
  )
})

test_that("ffp_moments does not accept external probabilities for fitted objects", {
  scenarios <- data.frame(
    x = c(
      -1,
      0,
      1
    )
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_fit()

  expect_error(
    ffp_moments(
      fit,
      p = rep(
        1 / 3,
        3
      )
    ),
    "must not be supplied"
  )

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.5
  )

  expect_error(
    ffp_moments(
      pooled,
      p = rep(
        1 / 3,
        3
      )
    ),
    "must not be supplied"
  )
})
