make_joint_distribution_test_model <- function() {
  scenarios <- data.frame(
    equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    bonds = c(-0.02, 0.00, 0.01, 0.015, 0.025),
    inflation = c(0.02, 0.03, 0.035, 0.045, 0.05)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior_uniform())
}


test_that("view_joint_distribution() exposes the intended public API", {
  expect_identical(
    names(formals(view_joint_distribution)),
    c("x", "target", "order")
  )
})


test_that("view_joint_distribution() creates a view specification", {
  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  view <- view_joint_distribution(
    x = cbind(equity, bonds),
    target = target
  )

  expect_s3_class(
    view,
    "ffp_view_joint_distribution_spec"
  )
  expect_s3_class(
    view,
    "ffp_view_spec"
  )

  expect_identical(
    view$method,
    "joint_distribution"
  )
  expect_identical(
    view$target,
    target
  )
  expect_identical(
    view$parameters$order,
    2L
  )
  expect_true(
    inherits(view$x, "quosure")
  )
})


test_that("view_joint_distribution() requires x and target", {
  target <- data.frame(
    x = c(-1, 0, 1),
    y = c(0, 1, 2)
  )

  expect_error(
    view_joint_distribution(
      target = target
    ),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_joint_distribution(
      x = cbind(equity, bonds)
    ),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_joint_distribution() reuses distribution-view order validation", {
  target <- data.frame(
    x = c(-1, 0, 1),
    y = c(0, 1, 2)
  )

  expect_identical(
    view_joint_distribution(
      cbind(x, y),
      target,
      order = 2
    )$parameters$order,
    2L
  )

  expect_identical(
    view_joint_distribution(
      cbind(x, y),
      target,
      order = 4L
    )$parameters$order,
    4L
  )

  expect_error(
    view_joint_distribution(
      cbind(x, y),
      target,
      order = 1L
    ),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_joint_distribution(
      cbind(x, y),
      target,
      order = 5L
    ),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("joint target may have a different number of observations", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.07, -0.02, 0.01, 0.05, 0.09, 0.12),
    bonds = c(-0.04, -0.01, 0.00, 0.01, 0.02, 0.04, 0.05)
  )

  bound_model <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds),
        target = target
      )
    )

  view <- bound_model$views[[1]]

  expect_s3_class(
    view,
    "ffp_view_joint_distribution"
  )
  expect_s3_class(
    view,
    "ffp_view"
  )

  expect_equal(
    view$features$values,
    cbind(
      equity = model$scenarios$equity,
      bonds = model$scenarios$bonds
    )
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


test_that("joint distribution requires at least two features", {
  model <- make_joint_distribution_test_model()

  target <- c(
    -0.10,
    -0.02,
    0.03,
    0.09
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          x = equity,
          target = target
        )
      ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("joint target variables must match feature count", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04),
    inflation = c(0.01, 0.02, 0.03, 0.04)
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          x = cbind(equity, bonds),
          target = target
        )
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("joint target names and order must match x", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    bonds = c(-0.03, 0.00, 0.02, 0.04),
    equity = c(-0.10, -0.02, 0.03, 0.09)
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          x = cbind(equity, bonds),
          target = target
        )
      ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("joint targets must contain finite numeric values", {
  model <- make_joint_distribution_test_model()

  missing_target <- data.frame(
    equity = c(-0.10, NA_real_, 0.03),
    bonds = c(-0.02, 0.01, 0.03)
  )

  infinite_target <- data.frame(
    equity = c(-0.10, Inf, 0.03),
    bonds = c(-0.02, 0.01, 0.03)
  )

  character_target <- data.frame(
    equity = c("low", "middle", "high"),
    bonds = c(-0.02, 0.01, 0.03)
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          cbind(equity, bonds),
          target = missing_target
        )
      ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          cbind(equity, bonds),
          target = infinite_target
        )
      ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          cbind(equity, bonds),
          target = character_target
        )
      ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("joint target must contain at least one observation", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = numeric(),
    bonds = numeric()
  )

  expect_error(
    model |>
      ffp_view(
        view_joint_distribution(
          cbind(equity, bonds),
          target = target
        )
      ),
    class = "ffp_error_invalid_view_target"
  )
})

test_that("joint distribution compiles first and second raw moments", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04)
  )

  model <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds),
        target = target
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
    mean(target$equity),
    mean(target$bonds),
    mean(target$equity^2),
    mean(target$equity * target$bonds),
    mean(target$bonds^2)
  )

  expect_equal(constraints$a_eq, expected_a_eq)
  expect_equal(constraints$b_eq, expected_b_eq)
})


test_that("joint distribution supports third-order cross-moments", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(equity = c(-0.10, -0.02, 0.03, 0.09), bonds = c(-0.03, 0.00, 0.02, 0.04))

  model <- model |>
    ffp_view(
      view_joint_distribution(
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
    equity * bonds,
    bonds^2,
    equity^3,
    equity^2 * bonds,
    equity * bonds^2,
    bonds^3
  )

  dimnames(expected_a_eq) <- NULL

  expected_b_eq <- c(
    mean(target$equity),
    mean(target$bonds),
    mean(target$equity^2),
    mean(target$equity * target$bonds),
    mean(target$bonds^2),
    mean(target$equity^3),
    mean(target$equity^2 * target$bonds),
    mean(target$equity * target$bonds^2),
    mean(target$bonds^3)
  )

  expect_equal(constraints$a_eq, expected_a_eq)
  expect_equal(constraints$b_eq, expected_b_eq)
})


test_that("joint distribution supports fourth-order cross-moments", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(equity = c(-0.10, -0.02, 0.03, 0.09), bonds = c(-0.03, 0.00, 0.02, 0.04))

  model <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 4L
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity
  bonds <- model$scenarios$bonds

  expected_fourth <- rbind(
    equity^4,
    equity^3 * bonds,
    equity^2 * bonds^2,
    equity * bonds^3,
    bonds^4
  )

  actual_fourth <- tail(constraints$a_eq, 5L)
  dimnames(actual_fourth) <- NULL

  expect_equal(actual_fourth, expected_fourth)
  expect_equal(
    tail(constraints$b_eq, 5L),
    c(
      mean(target$equity^4),
      mean(target$equity^3 * target$bonds),
      mean(target$equity^2 * target$bonds^2),
      mean(target$equity * target$bonds^3),
      mean(target$bonds^4)
    )
  )
})


test_that("joint distribution orders three-feature second moments consistently", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04),
    inflation = c(0.01, 0.025, 0.04, 0.055)
  )

  model <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds, inflation),
        target = target,
        order = 2L
      )
    )

  constraints <- compile_views(model)

  equity <- model$scenarios$equity
  bonds <- model$scenarios$bonds
  inflation <- model$scenarios$inflation

  expected_a_eq <- rbind(
    equity,
    bonds,
    inflation,
    equity^2,
    equity * bonds,
    equity * inflation,
    bonds^2,
    bonds * inflation,
    inflation^2
  )

  dimnames(expected_a_eq) <- NULL

  expect_equal(constraints$a_eq, expected_a_eq)
})


test_that("joint distribution reports the correct constraint count", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(
    equity = c(-0.10, -0.02, 0.03, 0.09),
    bonds = c(-0.03, 0.00, 0.02, 0.04),
    inflation = c(0.01, 0.025, 0.04, 0.055)
  )

  order_2 <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds, inflation),
        target = target,
        order = 2L
      )
    )

  order_3 <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds, inflation),
        target = target,
        order = 3L
      )
    )

  order_4 <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds, inflation),
        target = target,
        order = 4L
      )
    )

  expect_identical(view_constraint_count(order_2$views[[1]]), 9L)
  expect_identical(view_constraint_count(order_3$views[[1]]), 19L)
  expect_identical(view_constraint_count(order_4$views[[1]]), 34L)
})


test_that("joint distribution compiler creates informative metadata", {
  model <- make_joint_distribution_test_model()

  target <- data.frame(equity = c(-0.10, -0.02, 0.03, 0.09), bonds = c(-0.03, 0.00, 0.02, 0.04))

  model <- model |>
    ffp_view(
      view_joint_distribution(
        x = cbind(equity, bonds),
        target = target,
        order = 3L
      )
    )

  metadata <- compile_views(model)$metadata

  expect_identical(
    metadata$method,
    rep("joint_distribution", 9L)
  )

  expect_match(metadata$label[[1]], "^E\\[equity\\] = ")
  expect_match(metadata$label[[3]], "^E\\[equity\\^2\\] = ")
  expect_match(metadata$label[[4]], "^E\\[equity \\* bonds\\] = ")
  expect_match(metadata$label[[7]], "^E\\[equity\\^2 \\* bonds\\] = ")
})

test_that("ffp_fit reproduces joint target moments", {
  scenarios <- data.frame(
    equity = c(-0.10, -0.06, -0.03, 0.00, 0.02, 0.05, 0.08, 0.12),
    bonds = c(0.04, 0.02, -0.01, 0.01, 0.03, -0.02, 0.00, 0.05)
  )

  target <- scenarios[c(1, 2, 2, 3, 4, 4, 5, 6, 7, 8, 8), ]

  fit <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform()) |>
    ffp_view(
      view_joint_distribution(
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

  target_moments <- c(
    mean(target$equity),
    mean(target$bonds),
    mean(target$equity^2),
    mean(target$equity * target$bonds),
    mean(target$bonds^2)
  )

  expect_equal(
    posterior_moments,
    target_moments,
    tolerance = 1e-8
  )
})
