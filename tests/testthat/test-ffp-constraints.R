# Helpers -----------------------------------------------------------------

make_constraint_test_model <- function() {
  scenarios <- data.frame(
    equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
    recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior_uniform())
}


# Empty compiler ----------------------------------------------------------

test_that("compile_views() returns empty matrices when there are no views", {
  model <- make_constraint_test_model()

  constraints <- compile_views(model)

  expect_s3_class(constraints, "ffp_constraints")
  expect_identical(dim(constraints$a_eq), c(0L, 5L))
  expect_identical(dim(constraints$a_ineq), c(0L, 5L))
  expect_identical(constraints$b_eq, numeric())
  expect_identical(constraints$b_ineq, numeric())
  expect_identical(nrow(constraints$metadata), 0L)
  expect_identical(constraints$n_scenarios, 5L)
})


test_that("compile_views() requires a prior", {
  model <- ffp_model(
    data.frame(x = c(-1, 0, 1))
  )

  expect_error(
    compile_views(model),
    class = "ffp_error_missing_prior"
  )
})


# Mean compiler -----------------------------------------------------------

test_that("mean views compile to equality constraints", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = equity,
        target = 0.01
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    constraints$a_eq,
    matrix(
      model$scenarios$equity,
      nrow = 1L
    )
  )

  expect_identical(
    constraints$b_eq,
    0.01
  )

  expect_identical(
    dim(constraints$a_ineq),
    c(0L, 5L)
  )

  expect_identical(
    constraints$b_ineq,
    numeric()
  )
})


test_that("vectorized mean views create one row per feature", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_mean(
        x = panel,
        target = c(0.01, 0.04)
      )
    )

  constraints <- compile_views(model)

  expected_a_eq <- t(panel)
  dimnames(expected_a_eq) <- NULL

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_identical(
    constraints$b_eq,
    c(0.01, 0.04)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c("equity", "inflation")
  )
})


# Probability compiler ----------------------------------------------------

test_that("marginal probability views compile to indicator equalities", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_probability(
        x = equity < 0,
        target = 0.40
      )
    )

  constraints <- compile_views(model)

  expected <- matrix(
    as.numeric(model$scenarios$equity < 0),
    nrow = 1L
  )

  expect_equal(
    constraints$a_eq,
    expected
  )

  expect_identical(
    constraints$b_eq,
    0.40
  )
})


test_that("conditional probabilities use the linear Meucci formulation", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = recession
      )
    )

  constraints <- compile_views(model)

  event <- model$scenarios$equity < 0
  given <- model$scenarios$recession

  expected <- as.numeric(event & given) -
    0.70 * as.numeric(given)

  expect_equal(
    constraints$a_eq,
    matrix(
      expected,
      nrow = 1L
    )
  )

  expect_identical(
    constraints$b_eq,
    0
  )

  expect_identical(
    constraints$metadata$condition,
    "recession"
  )
})


test_that("one conditioning event can compile several probability targets", {
  model <- make_constraint_test_model()

  events <- cbind(
    loss = model$scenarios$equity < 0,
    high_inflation = model$scenarios$inflation > 0.025
  )

  model <- model |>
    ffp_view(
      view_probability(
        x = events,
        target = c(0.70, 0.40),
        given = recession
      )
    )

  constraints <- compile_views(model)

  given <- model$scenarios$recession

  expected <- rbind(
    as.numeric(events[, "loss"] & given) -
      0.70 * as.numeric(given),
    as.numeric(events[, "high_inflation"] & given) -
      0.40 * as.numeric(given)
  )

  expect_equal(
    constraints$a_eq,
    expected
  )

  expect_identical(
    constraints$b_eq,
    c(0, 0)
  )
})


test_that("conditional probability is checked against the current prior", {
  scenarios <- data.frame(
    event = c(TRUE, FALSE, TRUE, FALSE),
    conditioning = c(TRUE, TRUE, FALSE, FALSE)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_probability(
        x = event,
        target = 0.50,
        given = conditioning
      )
    )

  model <- model |>
    ffp_prior(
      prior_custom(
        c(0, 0, 0.5, 0.5)
      )
    )

  expect_error(
    compile_views(model),
    class = "ffp_error_zero_conditioning_probability"
  )
})


# Quantile compiler -------------------------------------------------------

test_that("quantile views compile to two inequalities per feature", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_quantile(
        x = equity,
        target = 0.01,
        level = 0.80
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity

  expected <- rbind(
    as.numeric(equity < 0.01),
    as.numeric(equity > 0.01)
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_equal(
    constraints$b_ineq,
    c(0.80, 0.20),
    tolerance = 1e-15
  )

  expect_identical(
    dim(constraints$a_eq),
    c(0L, 5L)
  )

  expect_identical(
    constraints$b_eq,
    numeric()
  )
})


test_that("quantile equality leaves mass at the target unrestricted", {
  scenarios <- data.frame(
    x = c(-2, 0, 0, 1, 3)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_quantile(
        x = x,
        target = 0,
        level = 0.60
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$a_ineq[1, ],
    c(1, 0, 0, 0, 0)
  )

  expect_identical(
    constraints$a_ineq[2, ],
    c(0, 0, 0, 1, 1)
  )

  expect_identical(
    constraints$b_ineq,
    c(0.60, 0.40)
  )
})


test_that("scalar quantile level expands across several features", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(0.01, 0.035),
        level = 0.80
      )
    )

  constraints <- compile_views(model)

  expected <- rbind(
    as.numeric(panel[, "equity"] < 0.01),
    as.numeric(panel[, "equity"] > 0.01),
    as.numeric(panel[, "inflation"] < 0.035),
    as.numeric(panel[, "inflation"] > 0.035)
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_equal(
    constraints$b_ineq,
    c(0.80, 0.20, 0.80, 0.20),
    tolerance = 1e-15
  )

  expect_identical(
    constraints$metadata$feature_1,
    c(
      "equity",
      "equity",
      "inflation",
      "inflation"
    )
  )
})


test_that("quantile levels can vary by feature", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(0.01, 0.035),
        level = c(0.80, 0.90)
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    constraints$b_ineq,
    c(0.80, 0.20, 0.90, 0.10),
    tolerance = 1e-15
  )
})


test_that("quantile constraint metadata preserves mathematical row order", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_quantile(
        x = equity,
        target = 0.01,
        level = 0.80
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$constraint_type,
    c("inequality", "inequality")
  )

  expect_identical(
    constraints$metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$view_constraint_index,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$method,
    c("quantile", "quantile")
  )
})


# Median compiler ---------------------------------------------------------

test_that("median views compile as quantile constraints at level 0.5", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_median(
        x = equity,
        target = 0.01
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity

  expected <- rbind(
    as.numeric(equity < 0.01),
    as.numeric(equity > 0.01)
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_identical(
    constraints$b_ineq,
    c(0.5, 0.5)
  )

  expect_identical(
    constraints$metadata$method,
    c("median", "median")
  )
})


test_that("vectorized median views create two rows per feature", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_median(
        x = panel,
        target = c(0.01, 0.035)
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    dim(constraints$a_ineq),
    c(4L, 5L)
  )

  expect_identical(
    constraints$b_ineq,
    rep(0.5, 4L)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c(
      "equity",
      "equity",
      "inflation",
      "inflation"
    )
  )
})


# Rank compiler -----------------------------------------------------------

test_that("named rank views compile adjacent expectation differences", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation,
    cash = rep(0, 5)
  )

  model <- model |>
    ffp_view(
      view_rank(
        x = panel,
        order = c("equity", "cash", "inflation")
      )
    )

  constraints <- compile_views(model)

  expected <- rbind(
    panel[, "cash"] - panel[, "equity"],
    panel[, "inflation"] - panel[, "cash"]
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_identical(
    constraints$b_ineq,
    c(0, 0)
  )
})


test_that("rank views can use positions and select a subset", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation,
    cash = rep(0, 5),
    spread = model$scenarios$equity -
      model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_rank(
        x = panel,
        order = c(4, 1, 3)
      )
    )

  constraints <- compile_views(model)

  expected <- rbind(
    panel[, 1] - panel[, 4],
    panel[, 3] - panel[, 1]
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_identical(
    constraints$b_ineq,
    c(0, 0)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c("spread", "equity")
  )

  expect_identical(
    constraints$metadata$feature_2,
    c("equity", "cash")
  )
})


test_that("rank compiler preserves the explicitly declared order", {
  model <- make_constraint_test_model()

  panel <- cbind(
    low = c(1, 1, 1, 1, 1),
    high = c(10, 10, 10, 10, 10),
    middle = c(5, 5, 5, 5, 5)
  )

  model <- model |>
    ffp_view(
      view_rank(
        x = panel,
        order = c("low", "high", "middle")
      )
    )

  constraints <- compile_views(model)

  expected <- rbind(
    panel[, "high"] - panel[, "low"],
    panel[, "middle"] - panel[, "high"]
  )

  expect_equal(
    constraints$a_ineq,
    expected
  )

  expect_identical(
    constraints$metadata$feature_1,
    c("low", "high")
  )

  expect_identical(
    constraints$metadata$feature_2,
    c("high", "middle")
  )
})


test_that("rank metadata maps adjacent economic statements", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation,
    cash = rep(0, 5)
  )

  model <- model |>
    ffp_view(
      view_rank(
        x = panel,
        order = c("equity", "cash", "inflation")
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$constraint_type,
    c("inequality", "inequality")
  )

  expect_identical(
    constraints$metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$view_constraint_index,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$method,
    c("rank", "rank")
  )
})

# Variance Compiler -------------------------------------------------------

test_that("variance views compile to second-moment equalities", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_variance(
        x,
        target = 2
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    nrow(constraints$a_eq),
    1L
  )

  expect_identical(
    nrow(constraints$a_ineq),
    0L
  )

  expect_equal(
    constraints$a_eq,
    matrix(
      c(4, 1, 0, 1, 4),
      nrow = 1L
    ),
    tolerance = 0
  )

  expect_equal(
    constraints$b_eq,
    2,
    tolerance = 1e-12
  )

  expect_identical(
    constraints$metadata$method,
    "variance"
  )

  expect_identical(
    constraints$metadata$constraint_type,
    "equality"
  )
})


test_that("variance views use the current prior mean as reference", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  prior <- c(
    0.05,
    0.10,
    0.15,
    0.30,
    0.40
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_variance(
        x,
        target = 1.5
      )
    )

  constraints <- compile_views(model)

  reference_mean <- sum(
    prior * scenarios$x
  )

  expect_equal(
    constraints$b_eq,
    reference_mean^2 + 1.5,
    tolerance = 1e-12
  )
})


test_that("variance views compile one equality per feature", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2),
    y = c(2, 1, 0, -1, -2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_variance(
        cbind(x, y),
        target = c(2, 3)
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    nrow(constraints$a_eq),
    2L
  )

  expect_identical(
    constraints$b_eq,
    c(2, 3)
  )

  expect_identical(
    constraints$metadata$method,
    c("variance", "variance")
  )

  expect_identical(
    constraints$metadata$view_constraint_index,
    c(1L, 2L)
  )
})


test_that("variance views work end to end through ffp_fit", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_variance(
        x,
        target = 1.5
      )
    )

  fit <- ffp_fit(model)

  reference_mean <- sum(
    fit$prior * scenarios$x
  )

  posterior_second_moment <- sum(
    fit$posterior * scenarios$x^2
  )

  expect_equal(
    posterior_second_moment,
    reference_mean^2 + 1.5,
    tolerance = 1e-8
  )

  expect_true(
    fit$solver$converged
  )

  diagnostics <- ffp_diagnostics(fit)

  expect_identical(
    diagnostics$method,
    "variance"
  )

  expect_identical(
    diagnostics$status,
    "satisfied"
  )
})

# Aggregation and provenance ----------------------------------------------

test_that("compile_views() aggregates constraints across semantic views", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = equity,
        target = 0.01
      ),
      view_probability(
        x = inflation > 0.04,
        target = 0.20
      )
    )

  constraints <- compile_views(model)

  expected <- rbind(
    model$scenarios$equity,
    as.numeric(model$scenarios$inflation > 0.04)
  )

  expect_equal(
    constraints$a_eq,
    expected
  )

  expect_identical(
    constraints$b_eq,
    c(0.01, 0.20)
  )

  expect_identical(
    constraints$metadata$view_index,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$method,
    c("mean", "probability")
  )
})


test_that("equalities and inequalities aggregate independently", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = equity,
        target = 0.01
      ),
      view_quantile(
        x = inflation,
        target = 0.035,
        level = 0.80
      ),
      view_probability(
        x = recession,
        target = 0.40
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    dim(constraints$a_eq),
    c(2L, 5L)
  )

  expect_identical(
    dim(constraints$a_ineq),
    c(2L, 5L)
  )

  expect_identical(
    constraints$b_eq,
    c(0.01, 0.40)
  )

  expect_equal(
    constraints$b_ineq,
    c(0.80, 0.20),
    tolerance = 1e-15
  )

  equality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "equality",
  ]

  inequality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "inequality",
  ]

  expect_identical(
    equality_metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    inequality_metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$view_index,
    c(1L, 2L, 2L, 3L)
  )
})


test_that("constraint metadata maps exactly to mathematical rows", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_mean(
        x = panel,
        target = c(0.01, 0.04)
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$constraint_type,
    c("equality", "equality")
  )

  expect_identical(
    constraints$metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$view_constraint_index,
    c(1L, 2L)
  )

  expect_identical(
    constraints$metadata$view_index,
    c(1L, 1L)
  )
})


# Validation --------------------------------------------------------------

test_that("constraint matrices must match the scenario support", {
  metadata <- new_constraint_metadata(
    type = "equality",
    method = "mean",
    feature_1 = "x",
    label = "E[x] = 0"
  )

  expect_error(
    new_ffp_constraints(
      n_scenarios = 3L,
      a_eq = matrix(1, nrow = 1L, ncol = 2L),
      b_eq = 0,
      metadata = metadata
    ),
    class = "ffp_error_invalid_constraints"
  )
})


test_that("constraint metadata must match matrix rows", {
  expect_error(
    new_ffp_constraints(
      n_scenarios = 3L,
      a_eq = matrix(1, nrow = 1L, ncol = 3L),
      b_eq = 0
    ),
    class = "ffp_error_invalid_constraints"
  )
})


# Printing ----------------------------------------------------------------

test_that("constraint printing is compact", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_mean(
        equity,
        target = 0.01
      )
    )

  constraints <- compile_views(model)

  output <- capture.output(
    print(constraints)
  )

  expect_true(
    any(grepl("<ffp_constraints>", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Scenarios:    5", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Equalities:   1", output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Inequalities: 0", output, fixed = TRUE))
  )
})

# Volatility compiler -----------------------------------------------------

test_that("volatility views compile to second-moment equalities", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_volatility(
        x = x,
        target = 1.50
      )
    )

  constraints <- compile_views(model)

  reference_mean <- sum(
    model$prior * scenarios$x
  )

  expected_a_eq <- matrix(
    scenarios$x^2,
    nrow = 1L
  )

  expected_b_eq <- reference_mean^2 + 1.50^2

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq,
    tolerance = 1e-15
  )

  expect_identical(
    dim(constraints$a_ineq),
    c(0L, 5L)
  )

  expect_identical(
    constraints$b_ineq,
    numeric()
  )
})


test_that("volatility uses the current prior reference mean", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 4)
  )

  prior <- c(
    0.05,
    0.10,
    0.15,
    0.20,
    0.50
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_volatility(
        x = x,
        target = 1.25
      )
    )

  constraints <- compile_views(model)

  reference_mean <- sum(
    prior * scenarios$x
  )

  expected_b_eq <- reference_mean^2 + 1.25^2

  expect_equal(
    constraints$b_eq,
    expected_b_eq,
    tolerance = 1e-15
  )
})


test_that("changing the prior changes volatility reference moments", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 4)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_volatility(
        x = x,
        target = 1
      )
    )

  first_constraints <- compile_views(model)

  new_prior <- c(
    0.05,
    0.05,
    0.10,
    0.20,
    0.60
  )

  model <- model |>
    ffp_prior(
      prior_custom(new_prior)
    )

  second_constraints <- compile_views(model)

  first_mean <- mean(scenarios$x)
  second_mean <- sum(new_prior * scenarios$x)

  expect_equal(
    first_constraints$b_eq,
    first_mean^2 + 1,
    tolerance = 1e-15
  )

  expect_equal(
    second_constraints$b_eq,
    second_mean^2 + 1,
    tolerance = 1e-15
  )

  expect_false(
    isTRUE(
      all.equal(
        first_constraints$b_eq,
        second_constraints$b_eq
      )
    )
  )
})


test_that("vectorized volatility creates one equality per feature", {
  model <- make_constraint_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_volatility(
        x = panel,
        target = c(0.05, 0.02)
      )
    )

  constraints <- compile_views(model)

  reference_means <- c(
    sum(model$prior * panel[, "equity"]),
    sum(model$prior * panel[, "inflation"])
  )

  expected_a_eq <- rbind(
    panel[, "equity"]^2,
    panel[, "inflation"]^2
  )

  expected_b_eq <- reference_means^2 +
    c(0.05, 0.02)^2

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq,
    tolerance = 1e-15
  )

  expect_identical(
    constraints$metadata$feature_1,
    c("equity", "inflation")
  )

  expect_identical(
    constraints$metadata$method,
    c("volatility", "volatility")
  )
})


test_that("zero volatility target remains a valid exact target", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_volatility(
        x = x,
        target = 0
      )
    )

  constraints <- compile_views(model)

  reference_mean <- sum(
    model$prior * scenarios$x
  )

  expect_equal(
    constraints$b_eq,
    reference_mean^2,
    tolerance = 1e-15
  )
})


test_that("volatility compiler does not add a hidden mean constraint", {
  model <- make_constraint_test_model() |>
    ffp_view(
      view_volatility(
        x = equity,
        target = 0.05
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    nrow(constraints$a_eq),
    1L
  )

  expect_identical(
    length(constraints$b_eq),
    1L
  )

  expect_identical(
    constraints$metadata$method,
    "volatility"
  )
})


test_that("reference moment helpers use probability rather than sample moments", {
  x <- c(-3, 0, 1, 8)

  prior <- c(
    0.10,
    0.20,
    0.30,
    0.40
  )

  expected_mean <- sum(prior * x)
  expected_second_moment <- sum(prior * x^2)
  expected_variance <- expected_second_moment - expected_mean^2

  expect_equal(
    reference_mean(x, prior),
    expected_mean,
    tolerance = 1e-15
  )

  expect_equal(
    reference_second_moment(x, prior),
    expected_second_moment,
    tolerance = 1e-15
  )

  expect_equal(
    reference_variance(x, prior),
    expected_variance,
    tolerance = 1e-15
  )

  expect_equal(
    reference_sd(x, prior),
    sqrt(expected_variance),
    tolerance = 1e-15
  )
})


test_that("reference moments preserve structural zeros", {
  x <- c(-1000, 1, 2, 3)

  prior <- c(
    0,
    0.2,
    0.3,
    0.5
  )

  expected_mean <- 0.2 * 1 +
    0.3 * 2 +
    0.5 * 3

  expect_equal(
    reference_mean(x, prior),
    expected_mean,
    tolerance = 1e-15
  )
})

# Matrix-view helpers -----------------------------------------------------

make_matrix_constraint_test_model <- function(
    prior = prior_uniform()
) {
  scenarios <- data.frame(
    DAX = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    FTSE = c(-0.06, -0.02, 0.00, 0.03, 0.05),
    CAC = c(-0.07, -0.01, 0.02, 0.03, 0.04)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior)
}


make_constraint_covariance_target <- function() {
  target <- matrix(
    c(
      0.0400, 0.0180, 0.0120,
      0.0180, 0.0225, 0.0090,
      0.0120, 0.0090, 0.0100
    ),
    nrow = 3,
    byrow = TRUE
  )

  dimnames(target) <- list(
    c("DAX", "FTSE", "CAC"),
    c("DAX", "FTSE", "CAC")
  )

  target
}


make_constraint_correlation_target <- function() {
  target <- matrix(
    c(
      1.00, 0.60, 0.40,
      0.60, 1.00, 0.50,
      0.40, 0.50, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  dimnames(target) <- list(
    c("DAX", "FTSE", "CAC"),
    c("DAX", "FTSE", "CAC")
  )

  target
}


# Matrix pair ordering ----------------------------------------------------

test_that("matrix constraint pairs use deterministic row-major order", {
  pairs <- matrix_constraint_pairs(3L)

  expected <- matrix(
    c(
      1L, 1L,
      1L, 2L,
      1L, 3L,
      2L, 2L,
      2L, 3L,
      3L, 3L
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("row", "col")
    )
  )

  expect_identical(
    pairs,
    expected
  )
})


test_that("matrix constraint pairs have K(K + 1) / 2 rows", {
  for (k in 2:8) {
    pairs <- matrix_constraint_pairs(k)

    expect_identical(
      nrow(pairs),
      as.integer(k * (k + 1L) / 2L)
    )
  }
})


# Covariance compiler -----------------------------------------------------

test_that("covariance compiles the independent matrix triangle", {
  model <- make_matrix_constraint_test_model()

  target <- make_constraint_covariance_target()

  model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  constraints <- compile_views(model)

  x <- model$scenarios

  expected_a_eq <- rbind(
    x$DAX^2,
    x$DAX * x$FTSE,
    x$DAX * x$CAC,
    x$FTSE^2,
    x$FTSE * x$CAC,
    x$CAC^2
  )

  means <- c(
    sum(model$prior * x$DAX),
    sum(model$prior * x$FTSE),
    sum(model$prior * x$CAC)
  )

  expected_b_eq <- c(
    means[1]^2 + target[1, 1],
    means[1] * means[2] + target[1, 2],
    means[1] * means[3] + target[1, 3],
    means[2]^2 + target[2, 2],
    means[2] * means[3] + target[2, 3],
    means[3]^2 + target[3, 3]
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq,
    tolerance = 1e-15
  )

  expect_identical(
    dim(constraints$a_eq),
    c(6L, 5L)
  )

  expect_identical(
    dim(constraints$a_ineq),
    c(0L, 5L)
  )
})


test_that("covariance metadata preserves triangular provenance", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$method,
    rep("covariance", 6L)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c(
      "DAX",
      "DAX",
      "DAX",
      "FTSE",
      "FTSE",
      "CAC"
    )
  )

  expect_identical(
    constraints$metadata$feature_2,
    c(
      "DAX",
      "FTSE",
      "CAC",
      "FTSE",
      "CAC",
      "CAC"
    )
  )

  expect_identical(
    constraints$metadata$constraint_row,
    seq_len(6L)
  )
})


test_that("covariance uses reference means from the current prior", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 3),
    y = c(4, 1, 0, -2)
  )

  target <- matrix(
    c(
      2.0, 0.5,
      0.5, 1.5
    ),
    nrow = 2,
    byrow = TRUE
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_covariance(
        x = cbind(x, y),
        target = target
      )
    )

  first_constraints <- compile_views(model)

  new_prior <- c(
    0.10,
    0.10,
    0.20,
    0.60
  )

  model <- model |>
    ffp_prior(
      prior_custom(new_prior)
    )

  second_constraints <- compile_views(model)

  first_means <- c(
    mean(scenarios$x),
    mean(scenarios$y)
  )

  second_means <- c(
    sum(new_prior * scenarios$x),
    sum(new_prior * scenarios$y)
  )

  expected_first <- c(
    first_means[1]^2 + target[1, 1],
    first_means[1] * first_means[2] + target[1, 2],
    first_means[2]^2 + target[2, 2]
  )

  expected_second <- c(
    second_means[1]^2 + target[1, 1],
    second_means[1] * second_means[2] + target[1, 2],
    second_means[2]^2 + target[2, 2]
  )

  expect_equal(
    first_constraints$b_eq,
    expected_first,
    tolerance = 1e-15
  )

  expect_equal(
    second_constraints$b_eq,
    expected_second,
    tolerance = 1e-15
  )

  expect_false(
    isTRUE(
      all.equal(
        first_constraints$b_eq,
        second_constraints$b_eq
      )
    )
  )
})


test_that("covariance allows zero-dispersion features", {
  scenarios <- data.frame(
    constant = rep(1, 5),
    variable = c(-2, -1, 0, 1, 2)
  )

  target <- matrix(
    c(
      0, 0,
      0, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_covariance(
        x = cbind(constant, variable),
        target = target
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    nrow(constraints$a_eq),
    3L
  )

  expect_equal(
    constraints$a_eq,
    rbind(
      scenarios$constant^2,
      scenarios$constant * scenarios$variable,
      scenarios$variable^2
    )
  )
})


# Correlation compiler ----------------------------------------------------

test_that("correlation compiles the independent matrix triangle", {
  model <- make_matrix_constraint_test_model()

  target <- make_constraint_correlation_target()

  model <- model |>
    ffp_view(
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  constraints <- compile_views(model)

  x <- model$scenarios

  expected_a_eq <- rbind(
    x$DAX^2,
    x$DAX * x$FTSE,
    x$DAX * x$CAC,
    x$FTSE^2,
    x$FTSE * x$CAC,
    x$CAC^2
  )

  means <- c(
    sum(model$prior * x$DAX),
    sum(model$prior * x$FTSE),
    sum(model$prior * x$CAC)
  )

  sds <- c(
    sqrt(sum(model$prior * (x$DAX - means[1])^2)),
    sqrt(sum(model$prior * (x$FTSE - means[2])^2)),
    sqrt(sum(model$prior * (x$CAC - means[3])^2))
  )

  expected_b_eq <- c(
    means[1]^2 +
      sds[1]^2 * target[1, 1],
    means[1] * means[2] +
      sds[1] * sds[2] * target[1, 2],
    means[1] * means[3] +
      sds[1] * sds[3] * target[1, 3],
    means[2]^2 +
      sds[2]^2 * target[2, 2],
    means[2] * means[3] +
      sds[2] * sds[3] * target[2, 3],
    means[3]^2 +
      sds[3]^2 * target[3, 3]
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq,
    tolerance = 1e-15
  )

  expect_identical(
    dim(constraints$a_eq),
    c(6L, 5L)
  )
})


test_that("correlation diagonal preserves reference second moments", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 4),
    y = c(3, 1, -1, 0, 2)
  )

  prior <- c(
    0.05,
    0.10,
    0.15,
    0.20,
    0.50
  )

  target <- matrix(
    c(
      1, 0.40,
      0.40, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(prior)
    ) |>
    ffp_view(
      view_correlation(
        x = cbind(x, y),
        target = target
      )
    )

  constraints <- compile_views(model)

  expected_x_second_moment <- sum(
    prior * scenarios$x^2
  )

  expected_y_second_moment <- sum(
    prior * scenarios$y^2
  )

  expect_equal(
    constraints$b_eq[[1]],
    expected_x_second_moment,
    tolerance = 1e-15
  )

  expect_equal(
    constraints$b_eq[[3]],
    expected_y_second_moment,
    tolerance = 1e-15
  )
})


test_that("correlation metadata preserves triangular provenance", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$method,
    rep("correlation", 6L)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c(
      "DAX",
      "DAX",
      "DAX",
      "FTSE",
      "FTSE",
      "CAC"
    )
  )

  expect_identical(
    constraints$metadata$feature_2,
    c(
      "DAX",
      "FTSE",
      "CAC",
      "FTSE",
      "CAC",
      "CAC"
    )
  )
})


test_that("correlation is revalidated against the current prior", {
  scenarios <- data.frame(
    first = c(0, 0, 1, 2),
    second = c(0, 0, 2, 4)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_correlation(
        x = cbind(first, second),
        target = diag(2)
      )
    )

  expect_s3_class(
    model$views[[1]],
    "ffp_view_correlation"
  )

  model <- model |>
    ffp_prior(
      prior_custom(
        c(0.5, 0.5, 0, 0)
      )
    )

  expect_error(
    compile_views(model),
    class = "ffp_error_undefined_correlation"
  )
})


test_that("correlation uses dispersions from the current prior", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 3),
    y = c(3, 1, 0, -1, -2)
  )

  target <- matrix(
    c(
      1, 0.60,
      0.60, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_correlation(
        x = cbind(x, y),
        target = target
      )
    )

  first_constraints <- compile_views(model)

  new_prior <- c(
    0.05,
    0.10,
    0.15,
    0.20,
    0.50
  )

  model <- model |>
    ffp_prior(
      prior_custom(new_prior)
    )

  second_constraints <- compile_views(model)

  expect_false(
    isTRUE(
      all.equal(
        first_constraints$b_eq,
        second_constraints$b_eq
      )
    )
  )
})


# Matrix-view aggregation -------------------------------------------------

test_that("covariance and correlation blocks aggregate cleanly", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    dim(constraints$a_eq),
    c(12L, 5L)
  )

  expect_identical(
    length(constraints$b_eq),
    12L
  )

  expect_identical(
    constraints$metadata$view_index,
    c(
      rep(1L, 6L),
      rep(2L, 6L)
    )
  )

  expect_identical(
    constraints$metadata$constraint_row,
    seq_len(12L)
  )

  expect_identical(
    constraints$metadata$method,
    c(
      rep("covariance", 6L),
      rep("correlation", 6L)
    )
  )
})


# Global compiler invariants ----------------------------------------------

test_that("all current view families compile in one model", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_probability(
        x = DAX > 0,
        target = 0.40
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_median(
        x = CAC,
        target = 0.02
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      ),
      view_volatility(
        x = DAX,
        target = 0.05
      ),
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  constraints <- compile_views(model)

  expect_s3_class(
    constraints,
    "ffp_constraints"
  )

  expect_identical(
    dim(constraints$a_eq),
    c(15L, 5L)
  )

  expect_identical(
    length(constraints$b_eq),
    15L
  )

  expect_identical(
    dim(constraints$a_ineq),
    c(6L, 5L)
  )

  expect_identical(
    length(constraints$b_ineq),
    6L
  )

  expect_identical(
    nrow(constraints$metadata),
    21L
  )
})


test_that("global compiler output contains only finite coefficients", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_volatility(
        x = CAC,
        target = 0.05
      ),
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      )
    )

  constraints <- compile_views(model)

  expect_true(
    all(is.finite(constraints$a_eq))
  )

  expect_true(
    all(is.finite(constraints$b_eq))
  )

  expect_true(
    all(is.finite(constraints$a_ineq))
  )

  expect_true(
    all(is.finite(constraints$b_ineq))
  )
})


test_that("constraint metadata maps exactly to global matrix rows", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_median(
        x = CAC,
        target = 0.02
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      ),
      view_volatility(
        x = DAX,
        target = 0.05
      ),
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  constraints <- compile_views(model)

  equality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "equality",
    ,
    drop = FALSE
  ]

  inequality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "inequality",
    ,
    drop = FALSE
  ]

  expect_identical(
    equality_metadata$constraint_row,
    seq_len(nrow(constraints$a_eq))
  )

  expect_identical(
    inequality_metadata$constraint_row,
    seq_len(nrow(constraints$a_ineq))
  )

  expect_identical(
    equality_metadata$view_index,
    c(
      1L,
      5L,
      rep(6L, 6L),
      rep(7L, 6L)
    )
  )

  expect_identical(
    inequality_metadata$view_index,
    c(
      2L, 2L,
      3L, 3L,
      4L, 4L
    )
  )
})


test_that("each semantic view contributes the expected constraint count", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_probability(
        x = DAX > 0,
        target = 0.40
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_median(
        x = CAC,
        target = 0.02
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      ),
      view_volatility(
        x = DAX,
        target = 0.05
      ),
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  constraints <- compile_views(model)

  counts <- table(
    factor(
      constraints$metadata$view_index,
      levels = seq_len(8L)
    )
  )

  expect_identical(
    unname(as.integer(counts)),
    c(
      1L,
      1L,
      2L,
      2L,
      2L,
      1L,
      6L,
      6L
    )
  )
})


test_that("compiler does not modify the model or bound views", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      )
    )

  original_model <- model

  invisible(
    compile_views(model)
  )

  expect_identical(
    model,
    original_model
  )
})


test_that("compilation is deterministic", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_volatility(
        x = CAC,
        target = 0.05
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_constraint_correlation_target()
      )
    )

  first <- compile_views(model)
  second <- compile_views(model)

  expect_identical(
    first,
    second
  )
})


test_that("prior-independent views remain unchanged when the prior changes", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_mean(
        x = DAX,
        target = 0.01
      ),
      view_quantile(
        x = FTSE,
        target = 0,
        level = 0.60
      ),
      view_median(
        x = CAC,
        target = 0.02
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      )
    )

  first <- compile_views(model)

  model <- model |>
    ffp_prior(
      prior_custom(
        c(
          0,
          0.10,
          0.20,
          0.30,
          0.40
        )
      )
    )

  second <- compile_views(model)

  expect_identical(
    first,
    second
  )
})


test_that("compiler keeps equality and inequality row spaces independent", {
  model <- make_matrix_constraint_test_model() |>
    ffp_view(
      view_quantile(
        x = DAX,
        target = 0,
        level = 0.60
      ),
      view_mean(
        x = FTSE,
        target = 0.01
      ),
      view_rank(
        x = cbind(DAX, FTSE, CAC),
        order = c("DAX", "FTSE", "CAC")
      ),
      view_probability(
        x = CAC > 0,
        target = 0.50
      )
    )

  constraints <- compile_views(model)

  equality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "equality",
    ,
    drop = FALSE
  ]

  inequality_metadata <- constraints$metadata[
    constraints$metadata$constraint_type == "inequality",
    ,
    drop = FALSE
  ]

  expect_identical(
    equality_metadata$constraint_row,
    c(1L, 2L)
  )

  expect_identical(
    equality_metadata$view_index,
    c(2L, 4L)
  )

  expect_identical(
    inequality_metadata$constraint_row,
    c(1L, 2L, 3L, 4L)
  )

  expect_identical(
    inequality_metadata$view_index,
    c(1L, 1L, 3L, 3L)
  )
})
