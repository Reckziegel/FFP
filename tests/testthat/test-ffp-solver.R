# Helpers -----------------------------------------------------------------

build_solver_test_problem <- function(model) {
  constraints <- compile_views(model)

  build_entropy_problem(
    model = model,
    constraints = constraints
  )
}


# Solver control ----------------------------------------------------------

test_that("entropy_solver_control() creates valid defaults", {
  control <- entropy_solver_control()

  expect_identical(control$max_iterations, 10000L)
  expect_equal(control$relative_tolerance, 1e-10)
  expect_equal(control$gradient_tolerance, 1e-10)
  expect_equal(control$constraint_tolerance, 1e-8)
})


test_that("entropy_solver_control() rejects invalid values", {
  expect_error(
    entropy_solver_control(
      max_iterations = 0
    ),
    class = "ffp_error_invalid_solver_control"
  )

  expect_error(
    entropy_solver_control(
      constraint_tolerance = 0
    ),
    class = "ffp_error_invalid_solver_control"
  )
})


# No views ----------------------------------------------------------------

test_that("solver returns the prior exactly when there are no views", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  prior <- c(
    0.05,
    0.10,
    0.20,
    0.25,
    0.40
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  expect_s3_class(result, "ffp_solver_result")
  expect_identical(result$posterior, model$prior)
  expect_identical(result$objective, 0)
  expect_true(result$converged)

  expect_identical(
    result$status,
    "prior_satisfies_constraints"
  )

  expect_identical(
    result$backend,
    "none"
  )
})


test_that("zero prior probabilities remain represented without views", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  prior <- c(
    0,
    0,
    0.20,
    0.30,
    0.50
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  expect_identical(
    length(result$posterior),
    5L
  )

  expect_identical(
    result$posterior,
    model$prior
  )
})


# Prior already satisfies views ------------------------------------------

test_that("solver returns the prior when it already satisfies the views", {
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
        target = 0
      )
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  expect_identical(
    result$posterior,
    model$prior
  )

  expect_identical(
    result$status,
    "prior_satisfies_constraints"
  )
})


# Analytical probability benchmark ---------------------------------------

test_that("probability view matches the analytical group solution", {
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
        target = 0.75
      )
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  expected <- c(
    0.375,
    0.375,
    0.125,
    0.125
  )

  expect_equal(
    result$posterior,
    expected,
    tolerance = 1e-7
  )

  expect_equal(
    sum(result$posterior),
    1,
    tolerance = 1e-8
  )

  expect_equal(
    sum(result$posterior[scenarios$event]),
    0.75,
    tolerance = 1e-8
  )

  expect_true(result$objective > 0)
  expect_true(result$converged)

  expect_identical(
    result$method,
    "BFGS"
  )
})


# Mean view ---------------------------------------------------------------

test_that("mean views are satisfied by the posterior", {
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

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  posterior_mean <- sum(
    result$posterior *
      scenarios$x
  )

  expect_equal(
    posterior_mean,
    0.75,
    tolerance = 1e-8
  )

  expect_equal(
    sum(result$posterior),
    1,
    tolerance = 1e-8
  )

  expect_true(
    all(result$posterior >= 0)
  )
})


# Inequalities ------------------------------------------------------------

test_that("quantile inequalities are satisfied by the posterior", {
  scenarios <- data.frame(
    x = c(-1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_quantile(
        x,
        target = 0,
        level = 0.75
      )
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  lhs <- drop(
    problem$a_ineq %*%
      result$posterior
  )

  expect_true(
    all(
      lhs <=
        problem$b_ineq + 1e-8
    )
  )

  expect_equal(
    sum(
      result$posterior[
        scenarios$x > 0
      ]
    ),
    0.25,
    tolerance = 1e-7
  )

  expect_identical(
    result$method,
    "L-BFGS-B"
  )
})


# Solver result diagnostics ----------------------------------------------

test_that("solver records posterior residual diagnostics", {
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
        target = 0.50
      )
    )

  problem <- build_solver_test_problem(model)

  result <- solve_entropy_problem(problem)

  expect_lte(
    result$residuals$normalization_residual,
    1e-8
  )

  expect_lte(
    result$residuals$max_equality_residual,
    1e-8
  )

  expect_lte(
    result$residuals$max_inequality_violation,
    1e-8
  )

  expect_gte(
    result$residuals$min_probability,
    0
  )
})


# Relative entropy --------------------------------------------------------

test_that("entropy_kl_divergence() is zero when posterior equals prior", {
  prior <- c(
    0.10,
    0.20,
    0.30,
    0.40
  )

  expect_equal(
    entropy_kl_divergence(
      posterior = prior,
      prior = prior
    ),
    0,
    tolerance = 1e-15
  )
})


test_that("entropy_kl_divergence() handles zero posterior mass", {
  prior <- c(
    0.25,
    0.25,
    0.25,
    0.25
  )

  posterior <- c(
    0,
    0.50,
    0.25,
    0.25
  )

  value <- entropy_kl_divergence(
    posterior = posterior,
    prior = prior
  )

  expect_true(is.finite(value))

  expect_equal(
    value,
    0.5 * log(2),
    tolerance = 1e-15
  )
})


test_that("entropy_kl_divergence() is infinite outside prior support", {
  prior <- c(
    0,
    0.50,
    0.50
  )

  posterior <- c(
    0.10,
    0.45,
    0.45
  )

  expect_identical(
    entropy_kl_divergence(
      posterior = posterior,
      prior = prior
    ),
    Inf
  )
})

# Posterior validation ----------------------------------------------------

test_that("posterior validation detects violated equalities", {
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

  problem <- build_solver_test_problem(model)

  invalid_posterior <- c(
    1 / 3,
    1 / 3,
    1 / 3
  )

  expect_error(
    validate_entropy_solution(
      problem = problem,
      posterior = invalid_posterior,
      tolerance = 1e-8
    ),
    class = "ffp_error_invalid_posterior"
  )
})


test_that("posterior validation detects invalid normalization", {
  scenarios <- data.frame(
    x = c(-1, 0, 1)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )

  problem <- build_solver_test_problem(model)

  invalid_posterior <- c(
    0.20,
    0.20,
    0.20
  )

  expect_error(
    validate_entropy_solution(
      problem = problem,
      posterior = invalid_posterior,
      tolerance = 1e-8
    ),
    class = "ffp_error_invalid_posterior"
  )
})


# Purity and determinism --------------------------------------------------

test_that("solver is deterministic and does not modify the problem", {
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
        target = 0.50
      )
    )

  problem <- build_solver_test_problem(model)
  original_problem <- problem

  first <- solve_entropy_problem(problem)
  second <- solve_entropy_problem(problem)

  expect_equal(
    first$posterior,
    second$posterior,
    tolerance = 1e-12
  )

  expect_equal(
    first$objective,
    second$objective,
    tolerance = 1e-12
  )

  expect_identical(
    problem,
    original_problem
  )
})
