# Helpers -----------------------------------------------------------------

make_opinion_pool_test_fit <- function() {
  scenarios <- data.frame(
    x = c(-1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 1
      )
    )

  ffp_fit(model)
}


# Public API ---------------------------------------------------------------

test_that("ffp_opinion_pooling() exposes the intended public API", {
  expect_identical(
    names(formals(ffp_opinion_pooling)),
    c("fit", "confidence")
  )
})


# Construction ------------------------------------------------------------

test_that("ffp_opinion_pooling() creates an opinion-pool object", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.25
  )

  expect_s3_class(pooled, "ffp_opinion_pool")

  expect_identical(
    names(pooled),
    c(
      "prior",
      "full_confidence_posterior",
      "posterior",
      "confidence",
      "fit",
      "n_scenarios"
    )
  )

  expect_identical(pooled$prior, fit$prior)
  expect_identical(
    pooled$full_confidence_posterior,
    fit$posterior
  )
  expect_identical(pooled$confidence, 0.25)
  expect_identical(pooled$fit, fit)
  expect_identical(pooled$n_scenarios, fit$n_scenarios)
})


# Pooling mathematics -----------------------------------------------------

test_that("global confidence applies linear opinion pooling", {
  fit <- make_opinion_pool_test_fit()

  confidence <- 0.25

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = confidence
  )

  expected <- (1 - confidence) * fit$prior +
    confidence * fit$posterior

  expect_equal(
    pooled$posterior,
    expected,
    tolerance = 0
  )
})


test_that("probability changes scale exactly with global confidence", {
  fit <- make_opinion_pool_test_fit()

  confidence <- 0.40

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = confidence
  )

  pooled_change <- pooled$posterior - pooled$prior

  full_confidence_change <-
    pooled$full_confidence_posterior - pooled$prior

  expect_equal(
    pooled_change,
    confidence * full_confidence_change,
    tolerance = 1e-15
  )
})


test_that("confidence zero returns the prior exactly", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0
  )

  expect_identical(
    pooled$posterior,
    fit$prior
  )
})


test_that("confidence one returns the full-confidence posterior exactly", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 1
  )

  expect_identical(
    pooled$posterior,
    fit$posterior
  )
})


test_that("pooling preserves an unchanged fit exactly", {
  scenarios <- data.frame(
    x = c(-1, 0, 1, 2)
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_fit()

  expect_identical(
    fit$posterior,
    fit$prior
  )

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.37
  )

  expect_identical(
    pooled$posterior,
    fit$prior
  )
})


test_that("opinion-pooled probabilities remain a probability distribution", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.63
  )

  expect_true(all(pooled$posterior >= 0))

  expect_equal(
    sum(pooled$posterior),
    1,
    tolerance = 1e-12
  )
})


# Validation ---------------------------------------------------------------

test_that("ffp_opinion_pooling() requires an ffp_fit", {
  expect_error(
    ffp_opinion_pooling(
      list(),
      confidence = 0.50
    ),
    class = "ffp_error_invalid_fit"
  )
})


test_that("ffp_opinion_pooling() requires confidence", {
  fit <- make_opinion_pool_test_fit()

  expect_error(
    ffp_opinion_pooling(fit),
    class = "ffp_error_invalid_confidence"
  )
})


test_that("confidence must be a numeric scalar", {
  fit <- make_opinion_pool_test_fit()

  invalid <- list(
    "0.50",
    TRUE,
    numeric(),
    c(0.25, 0.75),
    matrix(0.50)
  )

  for (confidence in invalid) {
    expect_error(
      ffp_opinion_pooling(
        fit,
        confidence = confidence
      ),
      class = "ffp_error_invalid_confidence"
    )
  }
})


test_that("confidence must be finite and non-missing", {
  fit <- make_opinion_pool_test_fit()

  invalid <- list(
    NA_real_,
    NaN,
    Inf,
    -Inf
  )

  for (confidence in invalid) {
    expect_error(
      ffp_opinion_pooling(
        fit,
        confidence = confidence
      ),
      class = "ffp_error_invalid_confidence"
    )
  }
})


test_that("confidence must lie between zero and one", {
  fit <- make_opinion_pool_test_fit()

  expect_error(
    ffp_opinion_pooling(
      fit,
      confidence = -0.01
    ),
    class = "ffp_error_invalid_confidence"
  )

  expect_error(
    ffp_opinion_pooling(
      fit,
      confidence = 1.01
    ),
    class = "ffp_error_invalid_confidence"
  )
})


test_that("integer boundary confidence is canonicalized to double", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 1L
  )

  expect_identical(
    pooled$confidence,
    1
  )

  expect_identical(
    pooled$posterior,
    fit$posterior
  )
})


# Object integrity ---------------------------------------------------------

test_that("opinion-pool validation detects inconsistent posterior probabilities", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.50
  )

  pooled$posterior <- pooled$prior

  expect_error(
    validate_ffp_opinion_pool(pooled),
    class = "ffp_error_invalid_opinion_pool"
  )
})


test_that("opinion-pool validation detects an inconsistent source fit", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.50
  )

  pooled$full_confidence_posterior <- pooled$prior

  expect_error(
    validate_ffp_opinion_pool(pooled),
    class = "ffp_error_invalid_opinion_pool"
  )
})


# Printing ----------------------------------------------------------------

test_that("opinion-pool printing is compact", {
  fit <- make_opinion_pool_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.25
  )

  output <- capture.output(
    print(pooled)
  )

  expect_true(
    any(grepl("<ffp_opinion_pool>", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Scenarios:   4", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Confidence:  25%", output, fixed = TRUE))
  )

  expect_false(
    any(grepl("KL divergence", output, fixed = TRUE))
  )

  expect_false(
    any(grepl("Max residual", output, fixed = TRUE))
  )
})
