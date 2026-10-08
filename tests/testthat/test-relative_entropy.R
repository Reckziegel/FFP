prior <- rep(1 / 100, 100)
posterior <- runif(100)
posterior <- posterior / sum(posterior)

re <- relative_entropy(prior, posterior)


test_that("relative_entropy returns a non-negative scalar", {
  expect_type(re, "double")
  expect_length(re, 1L)
  expect_gt(re, 0)
})


test_that("relative_entropy is zero for identical distributions", {
  p <- c(
    0.1,
    0.2,
    0.3,
    0.4
  )

  expect_equal(
    relative_entropy(
      prior = p,
      posterior = p
    ),
    0,
    tolerance = 1e-15
  )
})


test_that("zero posterior probabilities contribute zero entropy", {
  prior <- c(
    0.25,
    0.25,
    0.25,
    0.25
  )

  posterior <- c(
    0.5,
    0.5,
    0,
    0
  )

  expected <- (
    0.5 * log(0.5 / 0.25) +
      0.5 * log(0.5 / 0.25)
  )

  result <- relative_entropy(
    prior = prior,
    posterior = posterior
  )

  expect_equal(
    result,
    expected,
    tolerance = 1e-15
  )

  expect_true(
    is.finite(result)
  )
})


test_that("posterior mass outside prior support has infinite entropy", {
  prior <- c(
    0.5,
    0.5,
    0
  )

  posterior <- c(
    0.4,
    0.5,
    0.1
  )

  expect_identical(
    relative_entropy(
      prior = prior,
      posterior = posterior
    ),
    Inf
  )
})


test_that("shared structural zeros do not contribute to entropy", {
  prior <- c(
    0.5,
    0.5,
    0
  )

  posterior <- c(
    0.6,
    0.4,
    0
  )

  result <- relative_entropy(
    prior = prior,
    posterior = posterior
  )

  expected <- (
    0.6 * log(0.6 / 0.5) +
      0.4 * log(0.4 / 0.5)
  )

  expect_equal(
    result,
    expected,
    tolerance = 1e-15
  )

  expect_true(
    is.finite(result)
  )
})


test_that("relative_entropy validates probability distributions", {
  expect_error(
    relative_entropy(
      prior,
      c(prior, prior)
    )
  )

  expect_error(
    relative_entropy(
      prior,
      runif(100)
    )
  )
})


test_that("relative_entropy delegates to the FFP 2.0 KL definition", {
  prior <- c(
    0.1,
    0.2,
    0.3,
    0.4
  )

  posterior <- c(
    0.2,
    0.1,
    0.4,
    0.3
  )

  expected <- entropy_kl_divergence(
    posterior = posterior,
    prior = prior
  )

  result <- relative_entropy(
    prior = prior,
    posterior = posterior
  )

  expect_identical(
    result,
    expected
  )
})
