make_copula_test_model <- function() {
  scenarios <- data.frame(
    equity = c(0.10, 0.25, 0.45, 0.70, 0.90),
    bonds = c(0.20, 0.55, 0.30, 0.80, 0.65),
    inflation = c(0.15, 0.35, 0.60, 0.50, 0.85)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior_uniform())
}


test_that("view_copula() exposes the intended public API", {
  expect_identical(names(formals(view_copula)), c("x", "target", "order"))
})


test_that("view_copula() creates a view specification", {
  target <- data.frame(equity = c(0.10, 0.30, 0.70, 0.90), bonds = c(0.20, 0.60, 0.40, 0.80))

  view <- view_copula(x = cbind(equity, bonds), target = target)

  expect_s3_class(view, "ffp_view_copula_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "copula")
  expect_identical(view$target, target)
  expect_identical(view$parameters$order, 2L)
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_copula() requires x and target", {
  target <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.6, 0.8))

  expect_error(
    view_copula(target = target),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_copula(x = cbind(equity, bonds)),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_copula() reuses distribution-view order validation", {
  target <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.6, 0.8))

  expect_identical(view_copula(cbind(x, y), target, order = 2)$parameters$order, 2L)
  expect_identical(view_copula(cbind(x, y), target, order = 4L)$parameters$order, 4L)

  expect_error(
    view_copula(cbind(x, y), target, order = 1L),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_copula(cbind(x, y), target, order = 5L),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("copula target may have a different number of observations", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = c(0.05, 0.15, 0.30, 0.50, 0.65, 0.80, 0.95),
    bonds = c(0.10, 0.40, 0.20, 0.70, 0.55, 0.85, 0.90)
  )

  bound_model <- model |>
    ffp_view(
      view_copula(x = cbind(equity, bonds), target = target)
    )

  view <- bound_model$views[[1]]

  expect_s3_class(view, "ffp_view_copula")
  expect_s3_class(view, "ffp_view")
  expect_equal(view$target$values, target)
  expect_identical(view$parameters$order, 2L)
})


test_that("copula requires at least two features", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = c(0.10, 0.30, 0.60, 0.90),
    bonds = c(0.20, 0.40, 0.70, 0.80)
  )

  expect_error(
    model |>
      ffp_view(
        view_copula(
          x = equity,
          target = target
        )
      ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("copula values must lie in the unit interval", {
  model <- make_copula_test_model()

  invalid_target <- data.frame(
    equity = c(0.10, 0.40, 1.10),
    bonds = c(0.20, 0.50, 0.80)
  )

  expect_error(
    view_copula(
      x = cbind(equity, bonds),
      target = invalid_target
    ),
    class = "ffp_error_invalid_view_target"
  )

  invalid_x <- data.frame(
    equity = c(0.10, 0.25, 1.20, 0.70, 0.90),
    bonds = c(0.20, 0.55, 0.30, 0.80, 0.65)
  )

  expect_error(
    model |>
      ffp_view(
        view_copula(
          x = invalid_x,
          target = model$scenarios[c("equity", "bonds")]
        )
      ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("copula target variables must match feature count", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = c(0.10, 0.30, 0.70, 0.90),
    bonds = c(0.20, 0.60, 0.40, 0.80),
    inflation = c(0.15, 0.35, 0.65, 0.85)
  )

  expect_error(
    model |>
      ffp_view(
        view_copula(x = cbind(equity, bonds), target = target)
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("copula target names and order must match x", {
  model <- make_copula_test_model()

  target <- data.frame(bonds = c(0.20, 0.60, 0.40, 0.80), equity = c(0.10, 0.30, 0.70, 0.90))

  expect_error(
    model |>
      ffp_view(
        view_copula(x = cbind(equity, bonds), target = target)
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("copula targets must contain finite numeric values", {

  missing_target <- data.frame(
    equity = c(0.10, NA_real_, 0.90),
    bonds = c(0.20, 0.50, 0.80)
  )

  infinite_target <- data.frame(
    equity = c(0.10, Inf, 0.90),
    bonds = c(0.20, 0.50, 0.80)
  )

  character_target <- data.frame(
    equity = c("low", "middle", "high"),
    bonds = c(0.20, 0.50, 0.80)
  )

  expect_error(
    view_copula(
      x = cbind(equity, bonds),
      target = missing_target
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_copula(
      x = cbind(equity, bonds),
      target = infinite_target
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_copula(
      x = cbind(equity, bonds),
      target = character_target
    ),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("copula target must contain at least one observation", {
  model <- make_copula_test_model()

  target <- data.frame(equity = numeric(), bonds = numeric())

  expect_error(
    model |>
      ffp_view(
        view_copula(cbind(equity, bonds), target = target)
      ),
    class = "ffp_error_invalid_view_target"
  )
})

test_that("copula compiles theoretical marginals and target dependence", {
  model <- make_copula_test_model()

  target <- data.frame(equity = c(0.10, 0.30, 0.70, 0.90), bonds = c(0.20, 0.60, 0.40, 0.80))

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target,
        order = 2L
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity
  bonds <- model$scenarios$bonds

  expected_a_eq <- rbind(
    equity,
    bonds,
    equity^2,
    equity * bonds,
    bonds^2
  )

  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    1 / 2,
    1 / 2,
    1 / 3,
    mean(target$equity * target$bonds),
    1 / 3
  )

  expect_equal(constraints$a_eq, expected_a_eq)
  expect_equal(constraints$b_eq, expected_b_eq)
})

test_that("copula supports third-order dependence moments", {
  model <- make_copula_test_model()

  target <- data.frame(equity = c(0.10, 0.30, 0.70, 0.90), bonds = c(0.20, 0.60, 0.40, 0.80))

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target,
        order = 3L
      )
    )

  constraints <- compile_views(model)

  expected_b_eq <- c(
    1 / 2,
    1 / 2,
    1 / 3,
    mean(target$equity * target$bonds),
    1 / 3,
    1 / 4,
    mean(target$equity^2 * target$bonds),
    mean(target$equity * target$bonds^2),
    1 / 4
  )

  expect_equal(constraints$b_eq, expected_b_eq)
})

test_that("copula supports fourth-order dependence moments", {
  model <- make_copula_test_model()

  target <- data.frame(equity = c(0.10, 0.30, 0.70, 0.90), bonds = c(0.20, 0.60, 0.40, 0.80))

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target,
        order = 4L
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    tail(constraints$b_eq, 5L),
    c(
      1 / 5,
      mean(target$equity^3 * target$bonds),
      mean(target$equity^2 * target$bonds^2),
      mean(target$equity * target$bonds^3),
      1 / 5
    )
  )
})

test_that("copula pure marginal moments do not depend on target marginals", {
  model <- make_copula_test_model()

  target_a <- data.frame(equity = c(0.10, 0.20, 0.30, 0.40), bonds = c(0.20, 0.40, 0.60, 0.80))
  target_b <- data.frame(equity = c(0.60, 0.70, 0.80, 0.90), bonds = c(0.10, 0.30, 0.50, 0.70))

  fit_a <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target_a,
        order = 2L
      )
    )

  fit_b <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target_b,
        order = 2L
      )
    )

  b_eq_a <- compile_views(fit_a)$b_eq
  b_eq_b <- compile_views(fit_b)$b_eq

  expect_equal(b_eq_a[c(1L, 2L, 3L, 5L)], c(1 / 2, 1 / 2, 1 / 3, 1 / 3))
  expect_equal(b_eq_b[c(1L, 2L, 3L, 5L)], c(1 / 2, 1 / 2, 1 / 3, 1 / 3))

  expect_false(
    isTRUE(
      all.equal(
        b_eq_a[[4L]],
        b_eq_b[[4L]]
      )
    )
  )
})

test_that("copula preserves multi-feature moment ordering", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = c(0.10, 0.30, 0.70, 0.90),
    bonds = c(0.20, 0.60, 0.40, 0.80),
    inflation = c(0.15, 0.35, 0.65, 0.85)
  )

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds, inflation),
        target = target,
        order = 2L
      )
    )

  constraints <- compile_views(model)

  expected_b_eq <- c(
    1 / 2,
    1 / 2,
    1 / 2,
    1 / 3,
    mean(target$equity * target$bonds),
    mean(target$equity * target$inflation),
    1 / 3,
    mean(target$bonds * target$inflation),
    1 / 3
  )

  expect_equal(constraints$b_eq, expected_b_eq)
})

test_that("copula reports the correct constraint count", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = c(0.10, 0.30, 0.70, 0.90),
    bonds = c(0.20, 0.60, 0.40, 0.80),
    inflation = c(0.15, 0.35, 0.65, 0.85)
  )

  order_2 <- model |>
    ffp_view(view_copula(cbind(equity, bonds, inflation), target, order = 2L))

  order_3 <- model |>
    ffp_view(view_copula(cbind(equity, bonds, inflation), target, order = 3L))

  order_4 <- model |>
    ffp_view(view_copula(cbind(equity, bonds, inflation), target, order = 4L))

  expect_identical(view_constraint_count(order_2$views[[1]]), 9L)
  expect_identical(view_constraint_count(order_3$views[[1]]), 19L)
  expect_identical(view_constraint_count(order_4$views[[1]]), 34L)
})

test_that("copula compiler creates informative metadata", {
  model <- make_copula_test_model()

  target <- data.frame(equity = c(0.10, 0.30, 0.70, 0.90), bonds = c(0.20, 0.60, 0.40, 0.80))

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target,
        order = 3L
      )
    )

  metadata <- compile_views(model)$metadata

  expect_identical(metadata$method, rep("copula", 9L))
  expect_match(metadata$label[[1]], "^E\\[equity\\] = ")
  expect_match(metadata$label[[3]], "^E\\[equity\\^2\\] = ")
  expect_match(metadata$label[[4]], "^E\\[equity \\* bonds\\] = ")
  expect_match(metadata$label[[7]], "^E\\[equity\\^2 \\* bonds\\] = ")
})

test_that("ffp_fit reproduces copula marginal and dependence moments", {
  scenarios <- data.frame(
    equity = c(0, 0, 0, 0.5, 0.5, 0.5, 1, 1, 1),
    bonds = c(0, 0.5, 1, 0, 0.5, 1, 0, 0.5, 1)
  )

  target_counts <- c(2L, 3L, 1L, 3L, 17L, 4L, 1L, 4L, 1L)

  target <- scenarios[
    rep(seq_len(nrow(scenarios)), times = target_counts),
    ,
    drop = FALSE
  ]

  fit <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target,
        order = 2L
      )
    ) |>
    ffp_fit()

  posterior <- as.double(fit$posterior)

  posterior_moments <- c(
    sum(posterior * scenarios$equity),
    sum(posterior * scenarios$bonds),
    sum(posterior * scenarios$equity^2),
    sum(posterior * scenarios$equity * scenarios$bonds),
    sum(posterior * scenarios$bonds^2)
  )

  target_cross_moment <- mean(target$equity * target$bonds)

  expected_moments <- c(
    1 / 2,
    1 / 2,
    1 / 3,
    target_cross_moment,
    1 / 3
  )

  expect_equal(
    posterior_moments,
    expected_moments,
    tolerance = 1e-8
  )

  expect_identical(fit$n_views, 1L)
  expect_true(fit$solver$converged)
  expect_gt(fit$solver$objective, 0)
})

test_that("copula target supports a single observation", {
  model <- make_copula_test_model()

  target <- data.frame(
    equity = 0.70,
    bonds = 0.40
  )

  model <- model |>
    ffp_view(
      view_copula(
        x = cbind(equity, bonds),
        target = target
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    constraints$b_eq,
    c(
      1 / 2,
      1 / 2,
      1 / 3,
      0.70 * 0.40,
      1 / 3
    )
  )
})

test_that("copula printing is informative", {
  target <- data.frame(
    equity = c(0.10, 0.30, 0.70, 0.90),
    bonds = c(0.20, 0.60, 0.40, 0.80)
  )

  spec <- view_copula(
    x = cbind(equity, bonds),
    target = target,
    order = 2L
  )

  spec_output <- capture.output(
    print(spec)
  )

  expect_true(
    any(grepl("Method:      copula", spec_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Moment order: 2", spec_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Target obs.:  4", spec_output, fixed = TRUE))
  )

  model <- make_copula_test_model() |>
    ffp_view(spec)

  bound_output <- capture.output(
    print(model$views[[1]])
  )

  expect_true(
    any(grepl("<ffp_view_copula>", bound_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Features:    2", bound_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Constraints: 5", bound_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Moment order: 2", bound_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Target obs.:  4", bound_output, fixed = TRUE))
  )
})
