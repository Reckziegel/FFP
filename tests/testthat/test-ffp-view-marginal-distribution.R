make_marginal_distribution_test_model <- function() {
  scenarios <- data.frame(
    equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    bonds = c(-0.02, 0.00, 0.01, 0.015, 0.025)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior_uniform())
}


test_that("view_marginal_distribution() exposes the intended public API", {
  expect_identical(
    names(formals(view_marginal_distribution)),
    c("x", "target", "order")
  )
})


test_that("view_marginal_distribution() creates a view specification", {
  target <- c(-0.04, -0.01, 0.02, 0.05)

  view <- view_marginal_distribution(
    x = equity,
    target = target
  )

  expect_s3_class(
    view,
    "ffp_view_marginal_distribution_spec"
  )
  expect_s3_class(view, "ffp_view_spec")

  expect_identical(
    view$method,
    "marginal_distribution"
  )
  expect_identical(view$target, target)
  expect_identical(view$parameters$order, 2L)
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_marginal_distribution() requires x and target", {
  expect_error(
    view_marginal_distribution(
      target = c(-0.01, 0.01)
    ),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_marginal_distribution(
      x = equity
    ),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_marginal_distribution() validates order eagerly", {
  target <- c(-0.04, -0.01, 0.02, 0.05)

  expect_identical(
    view_marginal_distribution(
      equity,
      target,
      order = 2
    )$parameters$order,
    2L
  )

  expect_identical(
    view_marginal_distribution(
      equity,
      target,
      order = 4L
    )$parameters$order,
    4L
  )

  expect_error(
    view_marginal_distribution(
      equity,
      target,
      order = 1L
    ),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_marginal_distribution(
      equity,
      target,
      order = 5L
    ),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_marginal_distribution(
      equity,
      target,
      order = 2.5
    ),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_marginal_distribution(
      equity,
      target,
      order = NA_real_
    ),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("marginal targets may have a different number of observations", {
  model <- make_marginal_distribution_test_model()

  target <- c(
    -0.10,
    -0.07,
    -0.04,
    -0.01,
    0.01,
    0.03,
    0.06,
    0.10
  )

  bound_model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = equity,
        target = target
      )
    )

  view <- bound_model$views[[1]]

  expect_s3_class(
    view,
    "ffp_view_marginal_distribution"
  )
  expect_s3_class(view, "ffp_view")

  expect_equal(
    view$features$values,
    model$scenarios$equity
  )
  expect_equal(
    view$target$values,
    target
  )
  expect_identical(
    view$parameters$order,
    2L
  )
})


test_that("marginal targets support several variables", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  bound_model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 4L
      )
    )

  view <- bound_model$views[[1]]

  expect_equal(
    view$target$values,
    target
  )
  expect_identical(
    view$target$names,
    c("equity", "bonds")
  )
  expect_identical(
    view$parameters$order,
    4L
  )
})


test_that("marginal target variables must match feature count", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    only_one = c(-0.03, 0.00, 0.03)
  )

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          x = cbind(equity, bonds),
          target = target
        )
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("named marginal targets preserve feature identity and order", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    bonds = c(-0.03, 0.00, 0.03),
    equity = c(-0.08, 0.00, 0.08)
  )

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          x = cbind(equity, bonds),
          target = target
        )
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("marginal targets must contain finite numeric values", {
  model <- make_marginal_distribution_test_model()

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          equity,
          target = c(-0.02, NA_real_, 0.02)
        )
      ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          equity,
          target = c(-0.02, Inf, 0.02)
        )
      ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          equity,
          target = c("low", "middle", "high")
        )
      ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("marginal target must contain at least one observation", {
  model <- make_marginal_distribution_test_model()

  expect_error(
    model |>
      ffp_view(
        view_marginal_distribution(
          equity,
          target = numeric()
        )
      ),
    class = "ffp_error_invalid_view_target"
  )
})

test_that("marginal distribution compiles first and second raw moments", {
  model <- make_marginal_distribution_test_model()

  target <- c(
    -0.10,
    -0.02,
    0.03,
    0.09
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = equity,
        target = target
      )
    )

  constraints <- compile_views(model)

  x <- model$scenarios$equity

  expected_a_eq <- rbind(
    x,
    x^2
  )
  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    mean(target),
    mean(target^2)
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq
  )

  expect_identical(
    constraints$n_scenarios,
    5L
  )
})


test_that("marginal distribution supports third-order moments", {
  model <- make_marginal_distribution_test_model()

  target <- c(
    -0.10,
    -0.02,
    0.03,
    0.09
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = equity,
        target = target,
        order = 3L
      )
    )

  constraints <- compile_views(model)

  x <- model$scenarios$equity

  expected_a_eq <- rbind(
    x,
    x^2,
    x^3
  )
  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    mean(target),
    mean(target^2),
    mean(target^3)
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq
  )
})


test_that("marginal distribution supports fourth-order moments", {
  model <- make_marginal_distribution_test_model()

  target <- c(
    -0.10,
    -0.02,
    0.03,
    0.09
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = equity,
        target = target,
        order = 4L
      )
    )

  constraints <- compile_views(model)

  x <- model$scenarios$equity

  expected_a_eq <- rbind(
    x,
    x^2,
    x^3,
    x^4
  )
  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    mean(target),
    mean(target^2),
    mean(target^3),
    mean(target^4)
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq
  )
})


test_that("marginal distribution orders moments by degree then feature", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 3L
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity
  bonds <- model$scenarios$bonds

  expected_a_eq <- rbind(
    equity,
    bonds,
    equity^2,
    bonds^2,
    equity^3,
    bonds^3
  )
  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    mean(target$equity),
    mean(target$bonds),
    mean(target$equity^2),
    mean(target$bonds^2),
    mean(target$equity^3),
    mean(target$bonds^3)
  )

  expect_equal(
    constraints$a_eq,
    expected_a_eq
  )

  expect_equal(
    constraints$b_eq,
    expected_b_eq
  )
})


test_that("marginal distribution compiler creates informative metadata", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 2L
      )
    )

  constraints <- compile_views(model)

  expect_identical(
    constraints$metadata$method,
    rep("marginal_distribution", 4L)
  )

  expect_identical(
    constraints$metadata$feature_1,
    c(
      "equity",
      "bonds",
      "equity",
      "bonds"
    )
  )

  expect_match(
    constraints$metadata$label[[1]],
    "^E\\[equity\\] = "
  )

  expect_match(
    constraints$metadata$label[[3]],
    "^E\\[equity\\^2\\] = "
  )
})


test_that("marginal distribution reports the correct constraint count", {
  model <- make_marginal_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  model <- model |>
    ffp_view(
      view_marginal_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 4L
      )
    )

  view <- model$views[[1]]

  expect_identical(
    view_constraint_count(view),
    8L
  )
})

test_that("ffp_fit reproduces marginal target moments", {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2)
  )

  target <- c(
    -1.5,
    -0.5,
    0.5,
    1.5
  )

  fit <- ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_marginal_distribution(
        x = x,
        target = target,
        order = 2L
      )
    ) |>
    ffp_fit()

  posterior <- as.double(fit$posterior)

  posterior_mean <- sum(
    posterior * scenarios$x
  )

  posterior_second_moment <- sum(
    posterior * scenarios$x^2
  )

  expect_equal(
    posterior_mean,
    mean(target),
    tolerance = 1e-8
  )

  expect_equal(
    posterior_second_moment,
    mean(target^2),
    tolerance = 1e-8
  )
})
