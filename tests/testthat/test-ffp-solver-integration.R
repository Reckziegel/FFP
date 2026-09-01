# Helpers -----------------------------------------------------------------

solve_test_model <- function(model) {
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  solve_entropy_problem(problem)
}


expect_entropy_constraints_satisfied <- function(
    model,
    result,
    tolerance = 1e-8
) {
  constraints <- compile_views(model)

  equality_residuals <- drop(
    constraints$a_eq %*% result$posterior -
      constraints$b_eq
  )

  inequality_residuals <- drop(
    constraints$a_ineq %*% result$posterior -
      constraints$b_ineq
  )

  if (length(equality_residuals) > 0L) {
    expect_lte(
      max(abs(equality_residuals)),
      tolerance
    )
  }

  if (length(inequality_residuals) > 0L) {
    expect_lte(
      max(
        pmax(
          inequality_residuals,
          0
        )
      ),
      tolerance
    )
  }

  expect_equal(
    sum(result$posterior),
    1,
    tolerance = tolerance
  )

  expect_true(
    all(result$posterior >= 0)
  )

  invisible(result)
}


# Mean --------------------------------------------------------------------

test_that("mean views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 0.75
      )
    )

  result <- solve_test_model(model)

  posterior_mean <- sum(
    result$posterior *
      scenarios$x
  )

  expect_equal(
    posterior_mean,
    0.75,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Marginal probability ----------------------------------------------------

test_that("marginal probability views solve end-to-end", {
  scenarios <- data.frame(
    event = c(
      TRUE,
      TRUE,
      FALSE,
      FALSE
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_probability(
        event,
        target = 0.70
      )
    )

  result <- solve_test_model(model)

  posterior_probability <- sum(
    result$posterior[
      scenarios$event
    ]
  )

  expect_equal(
    posterior_probability,
    0.70,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Conditional probability -------------------------------------------------

test_that("conditional probability views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-1, 1, -1, 1),
    y = c(1, 1, -1, -1)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_probability(
        x > 0,
        target = 0.75,
        given = y > 0
      )
    )

  result <- solve_test_model(model)

  event <- scenarios$x > 0
  given <- scenarios$y > 0

  posterior_conditional <- sum(
    result$posterior[event & given]
  ) / sum(
    result$posterior[given]
  )

  expect_equal(
    posterior_conditional,
    0.75,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Quantile ----------------------------------------------------------------

test_that("quantile views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  prior <- c(
    0.35,
    0.30,
    0.10,
    0.15,
    0.10
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_quantile(
        x,
        target = 0,
        level = 0.50
      )
    )

  result <- solve_test_model(model)

  lower_probability <- sum(
    result$posterior[
      scenarios$x < 0
    ]
  )

  upper_probability <- sum(
    result$posterior[
      scenarios$x > 0
    ]
  )

  expect_lte(
    lower_probability,
    0.50 + 1e-8
  )

  expect_lte(
    upper_probability,
    0.50 + 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Median ------------------------------------------------------------------

test_that("median views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  prior <- c(
    0.45,
    0.30,
    0.05,
    0.15,
    0.05
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_median(
        x,
        target = 0
      )
    )

  result <- solve_test_model(model)

  lower_probability <- sum(
    result$posterior[
      scenarios$x < 0
    ]
  )

  upper_probability <- sum(
    result$posterior[
      scenarios$x > 0
    ]
  )

  expect_lte(
    lower_probability,
    0.50 + 1e-8
  )

  expect_lte(
    upper_probability,
    0.50 + 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Rank --------------------------------------------------------------------

test_that("rank views solve end-to-end", {
  scenarios <- data.frame(
    higher = c(0, 0, 1, 1),
    lower = c(1, 1, 0, 0)
  )

  prior <- c(
    0.40,
    0.40,
    0.10,
    0.10
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_rank(
        cbind(higher, lower),
        order = c("higher", "lower")
      )
    )

  result <- solve_test_model(model)

  higher_mean <- sum(
    result$posterior *
      scenarios$higher
  )

  lower_mean <- sum(
    result$posterior *
      scenarios$lower
  )

  expect_gte(
    higher_mean + 1e-8,
    lower_mean
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Volatility --------------------------------------------------------------

test_that("volatility views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-2, -1, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_volatility(
        x,
        target = 1.20
      )
    )

  result <- solve_test_model(model)

  prior_mean <- sum(
    model$prior *
      scenarios$x
  )

  posterior_second_moment <- sum(
    result$posterior *
      scenarios$x^2
  )

  expected_second_moment <- prior_mean^2 +
    1.20^2

  expect_equal(
    posterior_second_moment,
    expected_second_moment,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Covariance --------------------------------------------------------------

test_that("covariance views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-1, -1, 1, 1),
    y = c(-1, 1, -1, 1)
  )

  target <- matrix(
    c(
      1.0, 0.5,
      0.5, 1.0
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(
      c("x", "y"),
      c("x", "y")
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_covariance(
        cbind(x, y),
        target = target
      )
    )

  result <- solve_test_model(model)

  posterior_cross_moment <- sum(
    result$posterior *
      scenarios$x *
      scenarios$y
  )

  expect_equal(
    posterior_cross_moment,
    0.50,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Correlation -------------------------------------------------------------

test_that("correlation views solve end-to-end", {
  scenarios <- data.frame(
    x = c(-1, -1, 1, 1),
    y = c(-1, 1, -1, 1)
  )

  target <- matrix(
    c(
      1.0, -0.5,
      -0.5, 1.0
    ),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(
      c("x", "y"),
      c("x", "y")
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_correlation(
        cbind(x, y),
        target = target
      )
    )

  result <- solve_test_model(model)

  posterior_cross_moment <- sum(
    result$posterior *
      scenarios$x *
      scenarios$y
  )

  expect_equal(
    posterior_cross_moment,
    -0.50,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Multiple views ----------------------------------------------------------

test_that("multiple view families solve jointly", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2),
    event = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      ),
      view_probability(
        event,
        target = 0.70
      )
    )

  result <- solve_test_model(model)

  posterior_mean <- sum(
    result$posterior *
      scenarios$x
  )

  posterior_probability <- sum(
    result$posterior[
      scenarios$event
    ]
  )

  expect_equal(
    posterior_mean,
    0.50,
    tolerance = 1e-8
  )

  expect_equal(
    posterior_probability,
    0.70,
    tolerance = 1e-8
  )

  expect_entropy_constraints_satisfied(
    model,
    result
  )
})


# Minimum distortion ------------------------------------------------------

test_that("posterior has positive KL when views distort the prior", {
  scenarios <- data.frame(
    x = c(-1, 0, 1)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  result <- solve_test_model(model)

  expect_gt(
    result$objective,
    0
  )

  expect_equal(
    result$objective,
    entropy_kl_divergence(
      posterior = result$posterior,
      prior = model$prior
    ),
    tolerance = 1e-12
  )
})
