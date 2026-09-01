# Helpers -----------------------------------------------------------------

classify_test_model <- function(model) {
  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  classify_entropy_problem(problem)
}


# Regular -----------------------------------------------------------------

test_that("interior entropy problems are classified as regular", {
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

  classification <- classify_test_model(model)

  expect_identical(classification$category, "regular")
  expect_identical(classification$subtype, "interior")
  expect_true(classification$full_feasible)
  expect_true(classification$support_feasible)
  expect_gt(classification$interior_margin, 0)

})


# Infeasible --------------------------------------------------------------

test_that("impossible views are classified as infeasible", {
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

  classification <- classify_test_model(model)

  expect_identical(
    classification$category,
    "infeasible"
  )

  expect_identical(
    classification$subtype,
    "constraints"
  )

  expect_false(
    classification$full_feasible
  )

  expect_false(
    classification$support_feasible
  )

  expect_true(
    is.na(classification$interior_margin)
  )
})


# Boundary: posterior zero ------------------------------------------------

test_that("zero-probability targets are classified as simplex boundary", {
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

  classification <- classify_test_model(model)

  expect_identical(
    classification$category,
    "boundary"
  )

  expect_identical(
    classification$subtype,
    "simplex_face"
  )

  expect_true(
    classification$full_feasible
  )

  expect_true(
    classification$support_feasible
  )

  expect_lte(
    classification$interior_margin,
    1e-10
  )
})


test_that("extreme mean targets are classified as simplex boundary", {
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

  classification <- classify_test_model(model)

  expect_identical(
    classification$category,
    "boundary"
  )

  expect_identical(
    classification$subtype,
    "simplex_face"
  )

  expect_true(
    classification$full_feasible
  )

  expect_true(
    classification$support_feasible
  )

  expect_lte(
    classification$interior_margin,
    1e-10
  )
})


# Boundary: prior support -------------------------------------------------

test_that("views requiring zero-prior scenarios are prior-support boundary", {
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

  classification <- classify_test_model(model)

  expect_identical(
    classification$category,
    "boundary"
  )

  expect_identical(
    classification$subtype,
    "prior_support"
  )

  expect_true(
    classification$full_feasible
  )

  expect_false(
    classification$support_feasible
  )

  expect_true(
    is.na(classification$interior_margin)
  )

  expect_gt(
    classification$witness[[1]],
    0
  )
})


# Zero priors need not imply a boundary problem ---------------------------

test_that("zero prior entries do not automatically make a problem boundary", {
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

  classification <- classify_test_model(model)

  expect_identical(
    classification$category,
    "regular"
  )

  expect_identical(
    classification$subtype,
    "interior"
  )

  expect_true(
    classification$full_feasible
  )

  expect_true(
    classification$support_feasible
  )

  expect_gt(
    classification$interior_margin,
    0
  )
})


# Determinism -------------------------------------------------------------

test_that("entropy problem classification is deterministic and pure", {
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

  constraints <- compile_views(model)

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints
  )

  original_problem <- problem

  first <- classify_entropy_problem(problem)
  second <- classify_entropy_problem(problem)

  expect_identical(
    first,
    second
  )

  expect_identical(
    problem,
    original_problem
  )
})
