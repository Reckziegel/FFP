test_that("bootstrap_scenarios returns one row per draw", {
  x <- matrix(
    c(
      1, 10,
      2, 20,
      3, 30
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c(
        "equity",
        "bonds"
      )
    )
  )

  p <- as_ffp(
    c(
      0.2,
      0.3,
      0.5
    )
  )

  set.seed(123)

  result <- bootstrap_scenarios(
    x = x,
    p = p,
    n = 25
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_identical(
    dim(result),
    c(
      25L,
      2L
    )
  )

  expect_identical(
    names(result),
    c(
      "equity",
      "bonds"
    )
  )
})

test_that("bootstrap_scenarios respects zero and unit probabilities", {
  x <- data.frame(
    equity = c(
      -0.10,
      0.02,
      0.10
    ),
    bonds = c(
      0.05,
      0.01,
      -0.03
    )
  )

  p <- as_ffp(
    c(
      0,
      1,
      0
    )
  )

  result <- bootstrap_scenarios(
    x = x,
    p = p,
    n = 20
  )

  expect_identical(
    result$equity,
    rep(
      0.02,
      20
    )
  )

  expect_identical(
    result$bonds,
    rep(
      0.01,
      20
    )
  )
})

test_that("bootstrap_scenarios preserves complete joint scenarios", {
  x <- data.frame(
    equity = c(
      -0.10,
      0.00,
      0.10
    ),
    regime = c(
      "stress",
      "neutral",
      "growth"
    ),
    recession = c(
      TRUE,
      FALSE,
      FALSE
    )
  )

  p <- as_ffp(
    c(
      0,
      1,
      0
    )
  )

  result <- bootstrap_scenarios(
    x = x,
    p = p,
    n = 10
  )

  expect_identical(
    names(result),
    c(
      "equity",
      "regime",
      "recession"
    )
  )

  expect_identical(
    result$equity,
    rep(
      0,
      10
    )
  )

  expect_identical(
    result$regime,
    rep(
      "neutral",
      10
    )
  )

  expect_identical(
    result$recession,
    rep(
      FALSE,
      10
    )
  )
})

test_that("bootstrap_scenarios does not bootstrap the scenario index as a variable", {
  x <- data.frame(
    date = as.Date("2026-01-01") + 0:2,
    equity = c(
      -0.10,
      0.00,
      0.10
    ),
    bonds = c(
      0.03,
      0.02,
      0.01
    )
  )

  p <- as_ffp(
    c(
      0.2,
      0.3,
      0.5
    )
  )

  result <- bootstrap_scenarios(
    x = x,
    p = p,
    n = 10
  )

  expect_identical(
    names(result),
    c(
      "equity",
      "bonds"
    )
  )
})

test_that("bootstrap_scenarios uses posterior probabilities from ffp_fit", {
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

  set.seed(123)

  direct <- bootstrap_scenarios(
    x = fit$model$scenarios,
    p = as_ffp(
      fit$posterior
    ),
    n = 50
  )

  set.seed(123)

  fitted <- bootstrap_scenarios(
    fit,
    n = 50
  )

  expect_identical(
    fitted,
    direct
  )
})

test_that("bootstrap_scenarios uses the effective opinion-pooled posterior", {
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

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  set.seed(321)

  direct <- bootstrap_scenarios(
    x = pooled$fit$model$scenarios,
    p = as_ffp(
      pooled$posterior
    ),
    n = 50
  )

  set.seed(321)

  result <- bootstrap_scenarios(
    pooled,
    n = 50
  )

  expect_identical(
    result,
    direct
  )
})
