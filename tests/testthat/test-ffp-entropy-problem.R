# Helpers -----------------------------------------------------------------

make_entropy_problem_test_model <- function(prior = prior_uniform()) {
  scenarios <- data.frame(
    equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
    recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior)
}


# Construction ------------------------------------------------------------

test_that("build_entropy_problem() creates an entropy problem", {
  model <- make_entropy_problem_test_model()
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_s3_class(problem, "ffp_entropy_problem")
  expect_identical(problem$n_scenarios, 5L)
  expect_identical(problem$prior, model$prior)
})


test_that("normalization is added as the final equality", {
  model <- make_entropy_problem_test_model() |>
    ffp_view(
      view_mean(
        equity,
        target = 0
      )
    )

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  n_view_equalities <- nrow(constraints$a_eq)
  normalization_row <- n_view_equalities + 1L

  expect_identical(
    problem$a_eq[seq_len(n_view_equalities), , drop = FALSE],
    constraints$a_eq
  )

  expect_identical(
    problem$b_eq[seq_len(n_view_equalities)],
    constraints$b_eq
  )

  expect_equal(
    problem$a_eq[normalization_row, ],
    rep(1, problem$n_scenarios)
  )

  expect_identical(
    problem$b_eq[[normalization_row]],
    1
  )

  expect_identical(
    problem$metadata$normalization_row,
    normalization_row
  )
})


test_that("compiled inequalities pass through unchanged", {
  model <- make_entropy_problem_test_model() |>
    ffp_view(
      view_quantile(
        equity,
        target = 0.01,
        level = 0.60
      )
    )

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_identical(problem$a_ineq, constraints$a_ineq)
  expect_identical(problem$b_ineq, constraints$b_ineq)
  expect_identical(
    problem$metadata$constraints,
    constraints$metadata
  )
})


test_that("models without views produce a normalization-only problem", {
  model <- make_entropy_problem_test_model()
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_identical(dim(problem$a_eq), c(1L, 5L))
  expect_equal(problem$a_eq, matrix(1, nrow = 1L, ncol = 5L))
  expect_identical(problem$b_eq, 1)

  expect_identical(dim(problem$a_ineq), c(0L, 5L))
  expect_length(problem$b_ineq, 0L)

  expect_equal(
    nrow(problem$metadata$constraints),
    0L
  )

  expect_identical(
    problem$metadata$normalization_row,
    1L
  )
})


# Prior support -----------------------------------------------------------

test_that("prior zeros remain in the full scenario support", {
  prior <- prior_custom(
    c(0, 0, 0.20, 0.30, 0.50)
  )

  model <- make_entropy_problem_test_model(prior)
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_identical(problem$n_scenarios, 5L)
  expect_identical(problem$prior, model$prior)

  expect_equal(
    which(problem$prior == 0),
    c(1L, 2L)
  )

  expect_identical(ncol(problem$a_eq), 5L)
  expect_identical(ncol(problem$a_ineq), 5L)

  expect_false(
    "active_support" %in% names(problem)
  )
})


test_that("build_entropy_problem() does not modify the prior", {
  prior <- prior_custom(
    c(0, 0.10, 0.20, 0.30, 0.40)
  )

  model <- make_entropy_problem_test_model(prior)
  original_prior <- model$prior

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_identical(problem$prior, original_prior)
  expect_identical(model$prior, original_prior)
})


# Purity and determinism --------------------------------------------------

test_that("building the entropy problem is deterministic and pure", {
  model <- make_entropy_problem_test_model() |>
    ffp_view(
      view_mean(
        equity,
        target = 0
      ),
      view_quantile(
        inflation,
        target = 0.035,
        level = 0.50
      )
    )

  constraints <- compile_views(model)

  original_model <- model
  original_constraints <- constraints

  first <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  second <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  expect_identical(first, second)
  expect_identical(model, original_model)
  expect_identical(constraints, original_constraints)
})


# Validation --------------------------------------------------------------

test_that("constraints must match the model scenario support", {
  model <- make_entropy_problem_test_model()

  constraints <- new_ffp_constraints(
    n_scenarios = 4L
  )

  expect_error(
    build_entropy_problem(
      model = model,
      constraints = constraints
    ),
    class = "ffp_error_invalid_entropy_problem"
  )
})


test_that("entropy problems require a valid normalization constraint", {
  model <- make_entropy_problem_test_model()
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  invalid_lhs <- problem
  invalid_lhs$a_eq[
    invalid_lhs$metadata$normalization_row,
    1L
  ] <- 0

  expect_error(
    validate_ffp_entropy_problem(invalid_lhs),
    class = "ffp_error_invalid_entropy_problem"
  )

  invalid_rhs <- problem
  invalid_rhs$b_eq[
    invalid_rhs$metadata$normalization_row
  ] <- 0.99

  expect_error(
    validate_ffp_entropy_problem(invalid_rhs),
    class = "ffp_error_invalid_entropy_problem"
  )
})


test_that("normalization must remain the final equality", {
  model <- make_entropy_problem_test_model()
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  problem$metadata$normalization_row <- 2L

  expect_error(
    validate_ffp_entropy_problem(problem),
    class = "ffp_error_invalid_entropy_problem"
  )
})


test_that("entropy problems reject invalid prior probabilities", {
  model <- make_entropy_problem_test_model()
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  negative_prior <- problem
  negative_prior$prior[[1]] <- -0.10

  expect_error(
    validate_ffp_entropy_problem(negative_prior),
    class = "ffp_error_invalid_entropy_problem"
  )

  missing_prior <- problem
  missing_prior$prior[[1]] <- NA_real_

  expect_error(
    validate_ffp_entropy_problem(missing_prior),
    class = "ffp_error_invalid_entropy_problem"
  )
})


# Printing ----------------------------------------------------------------

test_that("entropy-problem printing is compact", {
  prior <- prior_custom(
    c(0, 0, 0.20, 0.30, 0.50)
  )

  model <- make_entropy_problem_test_model(prior) |>
    ffp_view(
      view_mean(
        equity,
        target = 0
      )
    )

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  output <- capture.output(
    print(problem)
  )

  expect_true(
    any(grepl("<ffp_entropy_problem>", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Scenarios:       5", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Prior zeros:     2", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("View equalities: 1", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Normalization:   yes", output, fixed = TRUE))
  )
})
