# Helpers -----------------------------------------------------------------

make_probability_path_test_fit <- function(with_index = FALSE) {
  if (with_index) {
    scenarios <- data.frame(
      date = as.Date("2026-01-01") + 0:3,
      x = c(-1, 0, 1, 2)
    )
  } else {
    scenarios <- data.frame(
      x = c(-1, 0, 1, 2)
    )
  }

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


# Full-confidence fit -----------------------------------------------------

test_that("ffp probabilities expose the full-confidence probability path", {
  fit <- make_probability_path_test_fit()

  probabilities <- ffp_probabilities(fit)

  expect_s3_class(
    probabilities,
    "tbl_df"
  )

  expect_identical(
    names(probabilities),
    c(
      "scenario",
      "index",
      "prior",
      "full_confidence",
      "difference"
    )
  )

  expect_identical(
    probabilities$scenario,
    seq_len(fit$n_scenarios)
  )

  expect_identical(
    probabilities$prior,
    fit$prior
  )

  expect_identical(
    probabilities$full_confidence,
    fit$posterior
  )

  expect_identical(
    probabilities$difference,
    fit$posterior - fit$prior
  )
})


test_that("full-confidence probability output has exactly five columns", {
  fit <- make_probability_path_test_fit()

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    ncol(probabilities),
    5L
  )

  expect_false(
    "posterior" %in% names(probabilities)
  )
})


# Opinion pooling ---------------------------------------------------------

test_that("opinion pooling appends posterior to the probability path", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  probabilities <- ffp_probabilities(pooled)

  expect_identical(
    names(probabilities),
    c(
      "scenario",
      "index",
      "prior",
      "full_confidence",
      "difference",
      "posterior"
    )
  )

  expect_identical(
    ncol(probabilities),
    6L
  )
})


test_that("opinion pooling preserves the fitted probability columns", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  fit_probabilities <- ffp_probabilities(fit)
  pooled_probabilities <- ffp_probabilities(pooled)

  expect_identical(
    pooled_probabilities[
      names(fit_probabilities)
    ],
    fit_probabilities
  )
})


test_that("opinion-pool posterior matches the pooled distribution", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.40
  )

  probabilities <- ffp_probabilities(pooled)

  expect_identical(
    probabilities$posterior,
    pooled$posterior
  )
})


test_that("difference keeps its full-confidence meaning after pooling", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.25
  )

  fit_probabilities <- ffp_probabilities(fit)
  pooled_probabilities <- ffp_probabilities(pooled)

  expect_identical(
    pooled_probabilities$difference,
    fit_probabilities$difference
  )

  expect_identical(
    pooled_probabilities$difference,
    pooled$full_confidence_posterior - pooled$prior
  )
})


# Confidence limits -------------------------------------------------------

test_that("confidence zero appends the prior as posterior", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0
  )

  probabilities <- ffp_probabilities(pooled)

  expect_identical(
    probabilities$posterior,
    probabilities$prior
  )

  expect_identical(
    probabilities$full_confidence,
    fit$posterior
  )
})


test_that("confidence one appends full confidence as posterior", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 1
  )

  probabilities <- ffp_probabilities(pooled)

  expect_identical(
    probabilities$posterior,
    probabilities$full_confidence
  )
})


test_that("partial-confidence posterior follows opinion pooling", {
  fit <- make_probability_path_test_fit()

  confidence <- 0.35

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = confidence
  )

  probabilities <- ffp_probabilities(pooled)

  expected <- probabilities$prior +
    confidence *
    (
      probabilities$full_confidence -
        probabilities$prior
    )

  expect_equal(
    probabilities$posterior,
    expected,
    tolerance = 1e-15
  )
})


# Scenario index ----------------------------------------------------------

test_that("probability path preserves the original scenario index", {
  fit <- make_probability_path_test_fit(
    with_index = TRUE
  )

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.60
  )

  fit_probabilities <- ffp_probabilities(fit)
  pooled_probabilities <- ffp_probabilities(pooled)

  expect_s3_class(
    fit_probabilities$index,
    "Date"
  )

  expect_identical(
    fit_probabilities$index,
    fit$model$metadata$index
  )

  expect_identical(
    pooled_probabilities$index,
    fit_probabilities$index
  )
})


test_that("probability path uses double NA when no index exists", {
  fit <- make_probability_path_test_fit()

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.60
  )

  fit_probabilities <- ffp_probabilities(fit)
  pooled_probabilities <- ffp_probabilities(pooled)

  expect_type(
    fit_probabilities$index,
    "double"
  )

  expect_true(
    all(is.na(fit_probabilities$index))
  )

  expect_identical(
    pooled_probabilities$index,
    fit_probabilities$index
  )
})


# Dispatch ---------------------------------------------------------------

test_that("ffp_probabilities() supports the existing named argument", {
  fit <- make_probability_path_test_fit()

  fit_probabilities <- ffp_probabilities(
    fit = fit
  )

  pooled <- ffp_opinion_pooling(
    fit,
    confidence = 0.50
  )

  pooled_probabilities <- ffp_probabilities(
    fit = pooled
  )

  expect_identical(
    fit_probabilities$full_confidence,
    fit$posterior
  )

  expect_identical(
    pooled_probabilities$posterior,
    pooled$posterior
  )
})


test_that("ffp_probabilities() retains invalid-fit validation", {
  expect_error(
    ffp_probabilities(
      list()
    ),
    class = "ffp_error_invalid_fit"
  )
})
