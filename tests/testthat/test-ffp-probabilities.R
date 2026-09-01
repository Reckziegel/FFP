# Helpers -----------------------------------------------------------------

make_probabilities_test_model <- function() {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )
}


# Output contract ---------------------------------------------------------

test_that("ffp_probabilities() returns a tibble with a stable schema", {
  model <- make_probabilities_test_model()
  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_s3_class(
    probabilities,
    "tbl_df"
  )

  expect_named(
    probabilities,
    c(
      "scenario",
      "index",
      "prior",
      "full_confidence",
      "difference"
    )
  )

  expect_identical(
    nrow(probabilities),
    fit$n_scenarios
  )

  expect_identical(
    ncol(probabilities),
    5L
  )
})


test_that("ffp_probabilities() preserves scenario positions", {
  model <- make_probabilities_test_model()
  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    probabilities$scenario,
    seq_len(fit$n_scenarios)
  )
})


test_that("ffp_probabilities() uses NA_real_ when no index is available", {
  model <- make_probabilities_test_model()
  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_type(
    probabilities$index,
    "double"
  )

  expect_true(
    all(is.na(probabilities$index))
  )

  expect_identical(
    length(probabilities$index),
    fit$n_scenarios
  )
})


test_that("ffp_probabilities() preserves a Date scenario index", {
  scenarios <- data.frame(
    date = as.Date("2026-01-01") + 0:4,
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_s3_class(
    probabilities$index,
    "Date"
  )

  expect_identical(
    probabilities$index,
    scenarios$date
  )

  expect_identical(
    ncol(probabilities),
    5L
  )
})


test_that("ffp_probabilities() preserves a character scenario index", {
  scenarios <- matrix(
    c(-2, -1, 0, 1, 2),
    ncol = 1L,
    dimnames = list(
      paste0("scenario_", 1:5),
      "x"
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    probabilities$index,
    rownames(scenarios)
  )

  expect_identical(
    ncol(probabilities),
    5L
  )
})


# Probability values ------------------------------------------------------

test_that("ffp_probabilities() returns prior and full-confidence probabilities", {
  model <- make_probabilities_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    probabilities$prior,
    fit$prior
  )

  expect_identical(
    probabilities$full_confidence,
    fit$posterior
  )

  expect_equal(
    probabilities$difference,
    fit$posterior - fit$prior,
    tolerance = 0
  )
})


test_that("ffp_probabilities() reports zero differences when full confidence equals prior", {
  model <- make_probabilities_test_model()
  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    probabilities$full_confidence,
    probabilities$prior
  )

  expect_identical(
    probabilities$difference,
    rep(0, fit$n_scenarios)
  )
})


test_that("ffp_probabilities() preserves exact zero prior probabilities", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(
        c(0, 0, 0.20, 0.30, 0.50)
      )
    )

  fit <- ffp_fit(model)

  probabilities <- ffp_probabilities(fit)

  expect_identical(
    probabilities$prior,
    c(0, 0, 0.20, 0.30, 0.50)
  )

  expect_identical(
    probabilities$full_confidence,
    probabilities$prior
  )

  expect_identical(
    which(probabilities$prior == 0),
    c(1L, 2L)
  )
})


# Validation --------------------------------------------------------------

test_that("ffp_probabilities() requires an ffp_fit object", {
  expect_error(
    ffp_probabilities(list()),
    class = "ffp_error_invalid_fit"
  )
})
