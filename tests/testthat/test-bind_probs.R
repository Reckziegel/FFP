test_that("bind_probs stacks named probability distributions", {
  prior <- as_ffp(
    c(
      0.25,
      0.25,
      0.25,
      0.25
    )
  )

  posterior <- as_ffp(
    c(
      0.10,
      0.20,
      0.30,
      0.40
    )
  )

  result <- bind_probs(
    prior = prior,
    posterior = posterior
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_named(
    result,
    c(
      "scenario",
      "distribution",
      "probability"
    )
  )

  expect_identical(
    nrow(result),
    8L
  )

  expect_identical(
    result$scenario,
    rep(
      1:4,
      2
    )
  )

  expect_identical(
    result$distribution,
    c(
      rep("prior", 4),
      rep("posterior", 4)
    )
  )

  expect_equal(
    result$probability,
    c(
      prior,
      posterior
    )
  )
})

test_that("bind_probs provides stable labels for unnamed distributions", {
  first <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  second <- as_ffp(
    c(
      0.25,
      0.75
    )
  )

  result <- bind_probs(
    first,
    second
  )

  expect_identical(
    unique(result$distribution),
    c(
      "distribution_1",
      "distribution_2"
    )
  )
})

test_that("bind_probs preserves exact zero probabilities", {
  first <- as_ffp(
    c(
      0,
      0.5,
      0.5
    )
  )

  second <- as_ffp(
    c(
      0.2,
      0,
      0.8
    )
  )

  result <- bind_probs(
    first = first,
    second = second
  )

  expect_identical(
    result$probability,
    c(
      0,
      0.5,
      0.5,
      0.2,
      0,
      0.8
    )
  )
})

test_that("bind_probs rejects non-ffp inputs", {
  probability <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  expect_error(
    bind_probs(
      valid = probability,
      invalid = as.double(probability)
    ),
    "must be"
  )
})

test_that("bind_probs requires common scenario size", {
  first <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  second <- as_ffp(
    c(
      0.2,
      0.3,
      0.5
    )
  )

  expect_error(
    bind_probs(
      first = first,
      second = second
    ),
    "same number of scenarios"
  )
})

test_that("bind_probs requires at least one distribution", {
  expect_error(
    bind_probs(),
    "at least one"
  )
})

test_that("bind_probs requires unique distribution names", {
  first <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  second <- as_ffp(
    c(
      0.4,
      0.6
    )
  )

  expect_error(
    bind_probs(
      distribution = first,
      distribution = second
    ),
    "must be unique"
  )
})
