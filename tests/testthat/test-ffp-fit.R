# Helpers -----------------------------------------------------------------

make_fit_test_model <- function() {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2),
    event = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )
}


# Basic fitting -----------------------------------------------------------

test_that("ffp_fit() returns an ffp_fit object", {
  model <- make_fit_test_model()

  fit <- ffp_fit(model)

  expect_s3_class(
    fit,
    "ffp_fit"
  )

  expect_identical(
    fit$n_scenarios,
    5L
  )

  expect_identical(
    fit$n_views,
    0L
  )

  expect_s3_class(
    fit$model,
    "ffp_model"
  )

  expect_s3_class(
    fit$constraints,
    "ffp_constraints"
  )

  expect_s3_class(
    fit$solver,
    "ffp_solver_result"
  )

  expect_identical(
    fit$model,
    model
  )

  expect_identical(
    fit$solver$control,
    entropy_solver_control()
  )
})


test_that("ffp_fit() returns the prior exactly when there are no views", {
  model <- make_fit_test_model()

  fit <- ffp_fit(model)

  expect_identical(
    fit$posterior,
    model$prior
  )

  expect_identical(
    fit$prior,
    model$prior
  )

  expect_identical(
    fit$solver$status,
    "prior_satisfies_constraints"
  )

  expect_identical(
    fit$solver$objective,
    0
  )
})


# Views -------------------------------------------------------------------

test_that("ffp_fit() incorporates mean views", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)

  posterior_mean <- sum(
    fit$posterior *
      model$scenarios$x
  )

  expect_equal(
    posterior_mean,
    0.50,
    tolerance = 1e-8
  )

  expect_identical(
    fit$n_views,
    1L
  )

  expect_true(
    fit$solver$converged
  )

  expect_gt(
    fit$solver$objective,
    0
  )
})


test_that("ffp_fit() incorporates multiple views", {
  model <- make_fit_test_model() |>
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

  fit <- ffp_fit(model)

  posterior_mean <- sum(
    fit$posterior *
      model$scenarios$x
  )

  posterior_probability <- sum(
    fit$posterior[
      model$scenarios$event
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

  expect_identical(
    fit$n_views,
    2L
  )
})


# Prior support -----------------------------------------------------------

test_that("ffp_fit() preserves zero prior entries when no view changes them", {
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

  expect_identical(
    fit$posterior,
    model$prior
  )

  expect_equal(
    which(fit$posterior == 0),
    c(1L, 2L)
  )

  expect_identical(
    length(fit$posterior),
    5L
  )
})


# Pipeline consistency ----------------------------------------------------

test_that("ffp_fit() matches the explicit internal pipeline", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  solver <- solve_entropy_problem(problem)

  fit <- ffp_fit(model)

  expect_identical(
    fit$constraints,
    constraints
  )

  expect_equal(
    fit$posterior,
    solver$posterior,
    tolerance = 1e-12
  )

  expect_equal(
    fit$solver$objective,
    solver$objective,
    tolerance = 1e-12
  )

  expect_identical(
    fit$solver$status,
    solver$status
  )
})


# Purity and determinism --------------------------------------------------

test_that("ffp_fit() does not modify the model", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  original_model <- model

  ffp_fit(model)

  expect_identical(
    model,
    original_model
  )
})


test_that("ffp_fit() is deterministic", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  first <- ffp_fit(model)
  second <- ffp_fit(model)

  expect_equal(
    first$posterior,
    second$posterior,
    tolerance = 1e-12
  )

  expect_equal(
    first$solver$objective,
    second$solver$objective,
    tolerance = 1e-12
  )

  expect_identical(
    first$solver$status,
    second$solver$status
  )
})


# Errors ------------------------------------------------------------------

test_that("ffp_fit() requires a prior", {
  scenarios <- data.frame(
    x = c(-1, 0, 1)
  )

  model <- ffp_model(scenarios)

  expect_error(
    ffp_fit(model),
    class = "ffp_error_missing_prior"
  )
})


test_that("ffp_fit validation detects inconsistent posterior data", {
  model <- make_fit_test_model()
  fit <- ffp_fit(model)

  invalid <- fit

  invalid$posterior <- c(
    0.10,
    0.10,
    0.20,
    0.20,
    0.40
  )

  expect_error(
    validate_ffp_fit(invalid),
    class = "ffp_error_invalid_fit"
  )
})


# Printing ----------------------------------------------------------------

test_that("ffp_fit printing is compact and informative", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)

  output <- capture.output(
    print(fit)
  )

  expect_true(
    any(
      grepl(
        "<ffp_fit>",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Scenarios:      5",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Views:          1",
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
        "Max residual:",
        output,
        fixed = TRUE
      )
    )
  )
})

# Fit provenance ----------------------------------------------------------

test_that("ffp_fit() preserves the fitted model", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)

  expect_s3_class(
    fit$model,
    "ffp_model"
  )

  expect_identical(
    fit$model,
    model
  )

  expect_identical(
    fit$prior,
    fit$model$prior
  )

  expect_identical(
    scenario_count(fit$model$scenarios),
    fit$n_scenarios
  )

  expect_identical(
    length(fit$model$views),
    fit$n_views
  )
})


test_that("ffp_fit() preserves the solver control used for fitting", {
  model <- make_fit_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  control <- entropy_solver_control(
    max_iterations = 1234L,
    relative_tolerance = 1e-11,
    gradient_tolerance = 1e-9,
    constraint_tolerance = 1e-7,
    refinement_iterations = 12L
  )

  fit <- ffp_fit(
    model,
    control = control
  )

  expect_identical(
    fit$solver$control,
    control
  )
})


test_that("ffp_fit() preserves solver control when prior satisfies constraints", {
  model <- make_fit_test_model()

  control <- entropy_solver_control(
    constraint_tolerance = 1e-9
  )

  fit <- ffp_fit(
    model,
    control = control
  )

  expect_identical(
    fit$solver$status,
    "prior_satisfies_constraints"
  )

  expect_identical(
    fit$solver$control,
    control
  )
})


test_that("ffp_fit validation detects an inconsistent stored model", {
  model <- make_fit_test_model()
  fit <- ffp_fit(model)

  incompatible_model <- ffp_model(
    data.frame(
      x = c(-1, 0, 1)
    )
  ) |>
    ffp_prior(
      prior_uniform()
    )

  invalid <- fit
  invalid$model <- incompatible_model

  expect_error(
    validate_ffp_fit(invalid),
    class = "ffp_error_invalid_fit"
  )
})


test_that("solver result validation detects invalid stored control", {
  model <- make_fit_test_model()
  fit <- ffp_fit(model)

  invalid <- fit
  invalid$solver$control$constraint_tolerance <- -1

  expect_error(
    validate_ffp_fit(invalid),
    class = "ffp_error_invalid_solver_result"
  )
})
