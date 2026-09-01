# Helpers -----------------------------------------------------------------

solve_classification_test_model <- function(model) {
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  solve_entropy_problem(problem)
}


# Regular -----------------------------------------------------------------

test_that("regular entropy problems continue to use the dual solver", {
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
        target = 0.25
      )
    )

  result <- solve_classification_test_model(model)

  expect_s3_class(
    result,
    "ffp_solver_result"
  )

  expect_true(
    result$converged
  )

  expect_identical(
    result$status,
    "converged"
  )

  expect_identical(
    result$backend,
    "stats::optim"
  )

  expect_equal(
    sum(
      result$posterior *
        scenarios$x
    ),
    0.25,
    tolerance = 1e-8
  )
})


# Infeasible --------------------------------------------------------------

test_that("infeasible problems receive a specific solver error", {
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
        target = 2
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_infeasible_problem"
  )
})


# Simplex boundary --------------------------------------------------------

test_that("zero probability targets are detected as simplex boundary", {
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
        target = 0
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_simplex_face"
  )
})


test_that("unit probability targets are detected as simplex boundary", {
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
        target = 1
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_simplex_face"
  )
})


test_that("extreme mean targets are detected as simplex boundary", {
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
        target = 1
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_simplex_face"
  )
})


# Prior-support boundary --------------------------------------------------

test_that("views requiring zero-prior scenarios report prior-support boundary", {
  scenarios <- data.frame(
    event = c(
      TRUE,
      FALSE,
      FALSE
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(
        c(0, 0.50, 0.50)
      )
    ) |>
    ffp_view(
      view_probability(
        event,
        target = 0.20
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_prior_support"
  )
})


# Zero prior with regular solution ----------------------------------------

test_that("zero prior entries do not prevent regular supported solutions", {
  scenarios <- data.frame(
    x = c(-10, -1, 0, 1)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(
        c(0, 0.30, 0.40, 0.30)
      )
    ) |>
    ffp_view(
      view_mean(
        x,
        target = 0.20
      )
    )

  result <- solve_classification_test_model(model)

  expect_s3_class(
    result,
    "ffp_solver_result"
  )

  expect_true(
    result$converged
  )

  expect_identical(
    result$posterior[[1]],
    0
  )

  expect_equal(
    sum(
      result$posterior *
        scenarios$x
    ),
    0.20,
    tolerance = 1e-8
  )
})


# Error hierarchy ---------------------------------------------------------

test_that("simplex boundary errors inherit from the general boundary class", {
  scenarios <- data.frame(
    event = c(
      TRUE,
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
        target = 0
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_problem"
  )
})


test_that("prior-support boundary errors inherit from the general boundary class", {
  scenarios <- data.frame(
    event = c(
      TRUE,
      FALSE
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(
        c(0, 1)
      )
    ) |>
    ffp_view(
      view_probability(
        event,
        target = 0.25
      )
    )

  expect_error(
    solve_classification_test_model(model),
    class = "ffp_error_boundary_problem"
  )
})
