# Helpers -----------------------------------------------------------------

make_summary_test_model <- function() {
  scenarios <- data.frame(
    x = c(-2, -1, 0, 1, 2),
    event = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  ffp_model(scenarios) |>
    ffp_prior(
      prior_uniform()
    )
}


# Summary object ----------------------------------------------------------

test_that("summary.ffp_fit() returns a fit summary object", {
  model <- make_summary_test_model()
  fit <- ffp_fit(model)

  result <- summary(fit)

  expect_s3_class(
    result,
    "summary_ffp_fit"
  )

  expect_named(
    result,
    c(
      "n_scenarios",
      "n_views",
      "status",
      "kl_divergence",
      "max_residual",
      "n_views_satisfied",
      "n_inequalities",
      "n_binding"
    )
  )
})


test_that("summary.ffp_fit() reports fitted model dimensions", {
  model <- make_summary_test_model() |>
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
  result <- summary(fit)

  expect_identical(
    result$n_scenarios,
    5L
  )

  expect_identical(
    result$n_views,
    2L
  )
})


test_that("summary.ffp_fit() reports view diagnostics", {
  model <- make_summary_test_model() |>
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
  result <- summary(fit)

  expect_identical(
    result$n_views_satisfied,
    2L
  )

  expect_identical(
    result$n_inequalities,
    2L
  )

  expect_identical(
    result$n_binding,
    1L
  )
})


test_that("summary.ffp_fit() reports KL divergence from the solver", {
  model <- make_summary_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)
  result <- summary(fit)

  expect_identical(
    result$kl_divergence,
    fit$solver$objective
  )

  expect_gt(
    result$kl_divergence,
    0
  )
})


test_that("summary.ffp_fit() reports the fit maximum residual", {
  model <- make_summary_test_model() |>
    ffp_view(
      view_mean(
        x,
        target = 0.50
      )
    )

  fit <- ffp_fit(model)
  result <- summary(fit)

  residuals <- fit$solver$residuals

  expected <- max(
    residuals$normalization_residual,
    residuals$max_equality_residual,
    residuals$max_inequality_violation
  )

  expect_identical(
    result$max_residual,
    expected
  )
})


test_that("summary.ffp_fit() handles fits without views", {
  model <- make_summary_test_model()
  fit <- ffp_fit(model)

  result <- summary(fit)

  expect_identical(
    result$n_views,
    0L
  )

  expect_identical(
    result$n_views_satisfied,
    0L
  )

  expect_identical(
    result$n_inequalities,
    0L
  )

  expect_identical(
    result$n_binding,
    0L
  )

  expect_identical(
    result$kl_divergence,
    0
  )
})


# Printing ----------------------------------------------------------------

test_that("fit summary printing is compact and informative", {
  model <- make_summary_test_model() |>
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
  result <- summary(fit)

  output <- capture.output(
    print(result)
  )

  expect_true(
    any(
      grepl(
        "<ffp_fit summary>",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Scenarios:       5",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Views:           2",
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

  expect_true(
    any(
      grepl(
        "Views satisfied: 2 / 2",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Inequalities:    2",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Binding:         1",
        output,
        fixed = TRUE
      )
    )
  )
})
