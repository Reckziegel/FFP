# Helpers -----------------------------------------------------------------

make_opinion_pool_summary_test_fit <- function() {
  scenarios <- data.frame(
    x = c(-1, 0, 1, 2)
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 1
      )
    ) |>
    ffp_fit()
}


# Summary object ----------------------------------------------------------

test_that("summary() creates an opinion-pool summary object", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  result <- summary(pooled)

  expect_s3_class(
    result,
    "summary_ffp_opinion_pool"
  )

  expect_identical(
    names(result),
    c(
      "n_scenarios",
      "n_views",
      "confidence",
      "kl_full_confidence",
      "kl_posterior",
      "mass_reallocated_full_confidence",
      "mass_reallocated_posterior"
    )
  )
})


test_that("summary() preserves structural information", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  result <- summary(pooled)

  expect_identical(
    result$n_scenarios,
    pooled$n_scenarios
  )

  expect_identical(
    result$n_views,
    fit$n_views
  )

  expect_identical(
    result$confidence,
    pooled$confidence
  )
})


# KL divergence -----------------------------------------------------------

test_that("summary() reports KL divergence from the prior", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  result <- summary(pooled)

  expect_identical(
    result$kl_full_confidence,
    entropy_kl_divergence(
      pooled$full_confidence_posterior,
      pooled$prior
    )
  )

  expect_identical(
    result$kl_posterior,
    entropy_kl_divergence(
      pooled$posterior,
      pooled$prior
    )
  )
})


test_that("partial-confidence KL does not exceed full-confidence KL", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  result <- summary(pooled)

  expect_lte(
    result$kl_posterior,
    result$kl_full_confidence
  )
})


# Probability mass reallocated -------------------------------------------

test_that("summary() reports total probability mass reallocated", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  result <- summary(pooled)

  expected_full <- 0.5 * sum(
    abs(
      pooled$full_confidence_posterior -
        pooled$prior
    )
  )

  expected_posterior <- 0.5 * sum(
    abs(
      pooled$posterior -
        pooled$prior
    )
  )

  expect_identical(
    result$mass_reallocated_full_confidence,
    expected_full
  )

  expect_identical(
    result$mass_reallocated_posterior,
    expected_posterior
  )
})


test_that("posterior mass reallocated scales with confidence", {
  fit <- make_opinion_pool_summary_test_fit()

  confidence <- 0.35

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = confidence
  )

  result <- summary(pooled)

  expect_equal(
    result$mass_reallocated_posterior,
    confidence *
      result$mass_reallocated_full_confidence,
    tolerance = 1e-15
  )
})


# Confidence limits -------------------------------------------------------

test_that("zero confidence produces zero posterior distortion", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0
  )

  result <- summary(pooled)

  expect_identical(
    result$kl_posterior,
    0
  )

  expect_identical(
    result$mass_reallocated_posterior,
    0
  )
})


test_that("full confidence reproduces full-confidence distortion", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 1
  )

  result <- summary(pooled)

  expect_identical(
    result$kl_posterior,
    result$kl_full_confidence
  )

  expect_identical(
    result$mass_reallocated_posterior,
    result$mass_reallocated_full_confidence
  )
})


test_that("unchanged probabilities produce zero distortion", {
  scenarios <- data.frame(
    x = c(-1, 0, 1, 2)
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.50
  )

  result <- summary(pooled)

  expect_identical(
    result$kl_full_confidence,
    0
  )

  expect_identical(
    result$kl_posterior,
    0
  )

  expect_identical(
    result$mass_reallocated_full_confidence,
    0
  )

  expect_identical(
    result$mass_reallocated_posterior,
    0
  )
})


# Printing ----------------------------------------------------------------

test_that("opinion-pool summary printing is informative", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  output <- capture.output(
    summary(pooled)
  )

  expect_true(
    any(
      grepl(
        "<ffp_opinion_pool summary>",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Scenarios:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Views:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Confidence:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "75%",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "KL divergence:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Probability mass reallocated:",
        output,
        fixed = TRUE
      )
    )
  )
})


test_that("opinion-pool summary does not mimic solver diagnostics", {
  fit <- make_opinion_pool_summary_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.75
  )

  output <- capture.output(
    summary(pooled)
  )

  expect_false(
    any(
      grepl(
        "Max residual",
        output,
        fixed = TRUE
      )
    )
  )

  expect_false(
    any(
      grepl(
        "Binding",
        output,
        fixed = TRUE
      )
    )
  )

  expect_false(
    any(
      grepl(
        "Converged",
        output,
        fixed = TRUE
      )
    )
  )
})
