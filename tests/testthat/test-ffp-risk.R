test_that("ffp_risk computes loss-oriented tail risk", {
  x <- c(
    -0.10,
    -0.05,
    0.00,
    0.05,
    0.10
  )

  risk <- ffp_risk(
    x,
    confidence = 0.80
  )

  out <- tibble::as_tibble(risk)

  expect_equal(
    out$value_at_risk,
    0.10
  )

  expect_equal(
    out$expected_shortfall,
    0.10
  )
})


test_that("ffp_risk uses flexible probabilities", {
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

  risk <- ffp_risk(
    x,
    p = p,
    confidence = 0.75
  )

  out <- tibble::as_tibble(risk)

  expect_equal(
    out$value_at_risk,
    1
  )

  expect_equal(
    out$expected_shortfall,
    1.8
  )
})

test_that("ffp_risk does not truncate profitable tails at zero", {
  x <- c(
    0.01,
    0.02,
    0.03,
    0.04
  )

  risk <- ffp_risk(
    x,
    confidence = 0.75
  )

  out <- tibble::as_tibble(risk)

  expect_equal(
    out$value_at_risk,
    -0.01
  )

  expect_equal(
    out$expected_shortfall,
    -0.01
  )
})

test_that("ffp_risk validates confidence", {
  expect_error(
    ffp_risk(
      1:5,
      confidence = 0
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    ffp_risk(
      1:5,
      confidence = 1
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    ffp_risk(
      1:5,
      confidence = c(
        0.95,
        0.99
      )
    ),
    "single finite number"
  )
})

test_that("ffp_risk works directly with ffp_fit", {
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

  risk <- ffp_risk(
    fit,
    confidence = 0.80
  )

  expect_s3_class(
    risk,
    "ffp_risk"
  )

  expect_identical(
    risk$type,
    "comparison"
  )

  expect_identical(
    risk$prior$variable,
    "equity"
  )

  expect_identical(
    risk$posterior$variable,
    "equity"
  )

  expect_equal(
    risk$change$value_at_risk,
    risk$posterior$value_at_risk -
      risk$prior$value_at_risk
  )

  expect_equal(
    risk$change$expected_shortfall,
    risk$posterior$expected_shortfall -
      risk$prior$expected_shortfall
  )
})

test_that("ffp_risk is unchanged when prior equals posterior", {
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
    )

  fit <- ffp_fit(model)

  risk <- ffp_risk(
    fit,
    confidence = 0.80
  )

  expect_true(
    all(
      risk$change[
        c(
          "value_at_risk",
          "expected_shortfall"
        )
      ] == 0
    )
  )

  expect_equal(
    risk$ens_retained,
    1
  )
})

test_that("ffp_risk rejects dimensional confidence values", {
  expect_error(
    ffp_risk(
      1:5,
      confidence = matrix(0.99)
    ),
    "single finite number"
  )
})

make_opinion_pool_stats_test_object <- function(
    confidence = 0.40
) {
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

  ffp_opinion_pooling(
    fit,
    confidence = confidence
  )
}

test_that("ffp_risk opinion pool preserves all three distributions", {
  pooled <- make_opinion_pool_stats_test_object()

  result <- ffp_risk(
    pooled,
    confidence = 0.80
  )

  fit_risk <- ffp_risk(
    pooled$fit,
    confidence = 0.80
  )

  posterior_risk <- ffp_risk(
    x = pooled$fit$model$scenarios,
    p = pooled$posterior,
    confidence = 0.80
  )

  expect_identical(
    result$type,
    "opinion_pool"
  )

  expect_equal(
    result$prior,
    fit_risk$prior
  )

  expect_equal(
    result$full_confidence,
    fit_risk$posterior
  )

  expect_equal(
    result$posterior,
    posterior_risk$risk
  )

  expect_identical(
    result$opinion_confidence,
    pooled$confidence
  )

  expect_identical(
    result$confidence,
    0.80
  )
})

test_that("ffp_risk opinion pooling respects confidence boundaries", {
  pooled_zero <- make_opinion_pool_stats_test_object(
    confidence = 0
  )

  pooled_one <- make_opinion_pool_stats_test_object(
    confidence = 1
  )

  risk_zero <- ffp_risk(
    pooled_zero,
    confidence = 0.80
  )

  risk_one <- ffp_risk(
    pooled_one,
    confidence = 0.80
  )

  expect_equal(
    risk_zero$posterior,
    risk_zero$prior
  )

  expect_equal(
    risk_one$posterior,
    risk_one$full_confidence
  )
})
