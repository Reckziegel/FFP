# Helpers -----------------------------------------------------------------

make_diagnostics_test_model <- function() {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2),
    event = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )
}


# Output contract ---------------------------------------------------------

test_that("constraint diagnostics return a stable empty tibble without views", {
  model <- make_diagnostics_test_model()
  fit <- ffp_fit(model)

  diagnostics <- compute_constraint_diagnostics(fit)

  expect_s3_class(
    diagnostics,
    "tbl_df"
  )

  expect_named(
    diagnostics,
    c(
      "view_id",
      "constraint_id",
      "method",
      "feature_1",
      "feature_2",
      "condition",
      "constraint_type",
      "constraint",
      "lhs",
      "rhs",
      "gap",
      "residual",
      "slack",
      "violation",
      "binding",
      "satisfied"
    )
  )

  expect_identical(
    nrow(diagnostics),
    0L
  )

  expect_identical(
    ncol(diagnostics),
    16L
  )
})


# Equalities --------------------------------------------------------------

test_that("constraint diagnostics evaluate equality constraints", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- compute_constraint_diagnostics(fit)

  expect_identical(
    nrow(diagnostics),
    1L
  )

  expect_identical(
    diagnostics$view_id,
    1L
  )

  expect_identical(
    diagnostics$constraint_id,
    1L
  )

  expect_identical(
    diagnostics$method,
    "mean"
  )

  expect_identical(
    diagnostics$constraint_type,
    "equality"
  )

  expect_equal(
    diagnostics$lhs,
    0.50,
    tolerance = 1e-8
  )

  expect_equal(
    diagnostics$rhs,
    0.50,
    tolerance = 0
  )

  expect_equal(
    diagnostics$gap,
    diagnostics$lhs - diagnostics$rhs,
    tolerance = 0
  )

  expect_equal(
    diagnostics$residual,
    abs(diagnostics$gap),
    tolerance = 0
  )

  expect_true(
    diagnostics$satisfied
  )

  expect_true(
    is.na(diagnostics$slack)
  )

  expect_true(
    is.na(diagnostics$violation)
  )

  expect_true(
    is.na(diagnostics$binding)
  )
})


# Inequalities ------------------------------------------------------------

test_that("constraint diagnostics evaluate inequality constraints", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- compute_constraint_diagnostics(fit)

  expect_identical(
    nrow(diagnostics),
    2L
  )

  expect_identical(
    diagnostics$constraint_type,
    c("inequality", "inequality")
  )

  expect_equal(
    diagnostics$lhs,
    c(0.20, 0.60),
    tolerance = 1e-12
  )

  expect_equal(
    diagnostics$rhs,
    c(0.20, 0.80),
    tolerance = 1e-12
  )

  expect_equal(
    diagnostics$slack,
    c(0, 0.20),
    tolerance = 1e-12
  )

  expect_equal(
    diagnostics$violation,
    c(0, 0),
    tolerance = 0
  )

  expect_identical(
    diagnostics$binding,
    c(TRUE, FALSE)
  )

  expect_true(
    all(diagnostics$satisfied)
  )

  expect_true(
    all(is.na(diagnostics$residual))
  )
})


# View mapping ------------------------------------------------------------

test_that("constraint diagnostics preserve view and constraint identities", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0
      ),
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- compute_constraint_diagnostics(fit)

  expect_identical(
    diagnostics$view_id,
    c(1L, 2L, 2L)
  )

  expect_identical(
    diagnostics$constraint_id,
    c(1L, 1L, 2L)
  )

  expect_identical(
    diagnostics$method,
    c("mean", "quantile", "quantile")
  )

  expect_identical(
    diagnostics$constraint_type,
    c("equality", "inequality", "inequality")
  )
})


# Conditional probabilities ----------------------------------------------

test_that("constraint diagnostics report compiled conditional probability constraints", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_probability(
        x > 0,
        target = 2 / 3,
        given = x >= 0
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- compute_constraint_diagnostics(fit)

  expect_identical(
    diagnostics$method,
    "probability"
  )

  expect_identical(
    diagnostics$constraint_type,
    "equality"
  )

  expect_equal(
    diagnostics$rhs,
    0,
    tolerance = 0
  )

  expect_equal(
    diagnostics$lhs,
    0,
    tolerance = 1e-12
  )

  expect_true(
    diagnostics$satisfied
  )

  expect_false(
    is.na(diagnostics$condition)
  )
})


# Solver tolerance --------------------------------------------------------

test_that("constraint binding uses the tolerance stored in the fit", {
  scenarios <- data.frame(
    x = 1:5
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_quantile(
        x,
        target = 3,
        level = 0.4000005
      )
    )

  loose_fit <- ffp_fit(
    model,
    control = entropy_solver_control(
      constraint_tolerance = 1e-6
    )
  )

  strict_fit <- ffp_fit(
    model,
    control = entropy_solver_control(
      constraint_tolerance = 1e-8
    )
  )

  loose <- compute_constraint_diagnostics(loose_fit)
  strict <- compute_constraint_diagnostics(strict_fit)

  expect_identical(
    loose_fit$solver$control$constraint_tolerance,
    1e-6
  )

  expect_identical(
    strict_fit$solver$control$constraint_tolerance,
    1e-8
  )

  expect_equal(
    loose$slack[[1]],
    strict$slack[[1]],
    tolerance = 1e-14
  )

  expect_gt(
    loose$slack[[1]],
    strict_fit$solver$control$constraint_tolerance
  )

  expect_lt(
    loose$slack[[1]],
    loose_fit$solver$control$constraint_tolerance
  )

  expect_true(
    loose$binding[[1]]
  )

  expect_false(
    strict$binding[[1]]
  )

  expect_true(
    all(loose$satisfied)
  )

  expect_true(
    all(strict$satisfied)
  )
})


# Validation --------------------------------------------------------------

test_that("constraint diagnostics require an ffp_fit object", {
  expect_error(
    compute_constraint_diagnostics(list()),
    class = "ffp_error_invalid_fit"
  )
})

# Public diagnostics ------------------------------------------------------

test_that("ffp_diagnostics() defaults to one row per view", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0
      ),
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- ffp_diagnostics(fit)

  expect_s3_class(
    diagnostics,
    "tbl_df"
  )

  expect_named(
    diagnostics,
    c(
      "view_id",
      "method",
      "expression",
      "n_constraints",
      "n_equalities",
      "n_inequalities",
      "n_binding",
      "max_error",
      "status"
    )
  )

  expect_identical(
    nrow(diagnostics),
    2L
  )

  expect_identical(
    diagnostics$view_id,
    c(1L, 2L)
  )

  expect_identical(
    diagnostics$method,
    c("mean", "quantile")
  )
})


test_that("view diagnostics aggregate mathematical constraints by view", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0
      ),
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- ffp_diagnostics(fit)

  expect_identical(
    diagnostics$n_constraints,
    c(1L, 2L)
  )

  expect_identical(
    diagnostics$n_equalities,
    c(1L, 0L)
  )

  expect_identical(
    diagnostics$n_inequalities,
    c(0L, 2L)
  )

  expect_identical(
    diagnostics$n_binding,
    c(0L, 1L)
  )

  expect_identical(
    diagnostics$status,
    c("satisfied", "satisfied")
  )
})


test_that("view diagnostics preserve original view expressions", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0
      ),
      view_probability(
        event,
        target = 0.60
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- ffp_diagnostics(fit)

  expect_identical(
    diagnostics$expression,
    c("x", "event")
  )
})


test_that("ffp_diagnostics() returns an empty view tibble without views", {
  model <- make_diagnostics_test_model()
  fit <- ffp_fit(model)

  diagnostics <- ffp_diagnostics(fit)

  expect_s3_class(
    diagnostics,
    "tbl_df"
  )

  expect_identical(
    nrow(diagnostics),
    0L
  )

  expect_identical(
    ncol(diagnostics),
    9L
  )

  expect_named(
    diagnostics,
    c(
      "view_id",
      "method",
      "expression",
      "n_constraints",
      "n_equalities",
      "n_inequalities",
      "n_binding",
      "max_error",
      "status"
    )
  )
})


test_that("ffp_diagnostics() exposes constraint-level diagnostics", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)

  public <- ffp_diagnostics(
    fit,
    level = "constraint"
  )

  internal <- compute_constraint_diagnostics(fit)

  expect_identical(
    public,
    internal
  )

  expect_identical(
    nrow(public),
    2L
  )
})


test_that("ffp_diagnostics() reports view max error from constraint diagnostics", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)

  view_diagnostics <- ffp_diagnostics(fit)

  constraint_diagnostics <- ffp_diagnostics(
    fit,
    level = "constraint"
  )

  expect_equal(
    view_diagnostics$max_error,
    max(constraint_diagnostics$residual, na.rm = TRUE),
    tolerance = 0
  )

  expect_identical(
    view_diagnostics$status,
    "satisfied"
  )
})


test_that("ffp_diagnostics() counts only inequalities as binding", {
  model <- make_diagnostics_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0
      ),
      view_quantile(
        x,
        target = -1,
        level = 0.20
      )
    )

  fit <- ffp_fit(model)
  diagnostics <- ffp_diagnostics(fit)

  expect_identical(
    diagnostics$n_binding[[1]],
    0L
  )

  expect_identical(
    diagnostics$n_binding[[2]],
    1L
  )
})


test_that("ffp_diagnostics() validates the diagnostic level", {
  model <- make_diagnostics_test_model()
  fit <- ffp_fit(model)

  expect_error(
    ffp_diagnostics(
      fit,
      level = "solver"
    )
  )
})


test_that("ffp_diagnostics() requires an ffp_fit object", {
  expect_error(
    ffp_diagnostics(list()),
    class = "ffp_error_invalid_fit"
  )
})
