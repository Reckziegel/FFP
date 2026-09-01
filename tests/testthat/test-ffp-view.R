# Helpers -----------------------------------------------------------------

make_view_test_model <- function(prior = prior_uniform()) {
  scenarios <- data.frame(
    equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
    recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior)
}


# Public API ---------------------------------------------------------------

test_that("view constructors expose the intended public API", {
  expect_identical(
    names(formals(view_mean)),
    c("x", "target")
  )

  expect_identical(
    names(formals(view_probability)),
    c("x", "target", "given")
  )

  expect_identical(
    names(formals(view_quantile)),
    c("x", "target", "level")
  )

  expect_identical(
    names(formals(view_median)),
    c("x", "target")
  )

  expect_identical(
    names(formals(view_rank)),
    c("x", "order")
  )

  expect_identical(
    names(formals(view_volatility)),
    c("x", "target")
  )
})


# view_mean(): specification ----------------------------------------------

test_that("view_mean() creates a mean-view specification", {
  view <- view_mean(
    x = equity,
    target = 0.01
  )

  expect_s3_class(view, "ffp_view_mean_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "mean")
  expect_identical(view$target, 0.01)
  expect_false("relation" %in% names(view))
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_mean() does not evaluate x at construction time", {
  view <- view_mean(
    x = stop("x should only be evaluated during binding"),
    target = 0
  )

  expect_s3_class(view, "ffp_view_mean_spec")

  model <- make_view_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_mean() requires x and target", {
  expect_error(
    view_mean(target = 0),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_mean(x = equity),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_mean() validates numeric targets eagerly", {
  expect_error(
    view_mean(equity, target = "0.01"),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_mean(equity, target = matrix(0.01)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_mean(equity, target = numeric()),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_mean(equity, target = c(0.01, NA_real_)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_mean(equity, target = Inf),
    class = "ffp_error_invalid_view_target"
  )
})


# ffp_view(): general binding ---------------------------------------------

test_that("ffp_view() requires an ffp_model with a prior", {
  view <- view_mean(equity, target = 0.01)

  expect_error(
    ffp_view(list(), view),
    class = "ffp_error_invalid_model"
  )

  model_without_prior <- ffp_model(
    data.frame(equity = c(-0.01, 0.01))
  )

  expect_error(
    ffp_view(model_without_prior, view),
    class = "ffp_error_missing_prior"
  )
})


test_that("ffp_view() requires at least one valid view specification", {
  model <- make_view_test_model()

  expect_error(
    ffp_view(model),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    ffp_view(model, 1),
    class = "ffp_error_invalid_view"
  )
})


test_that("ffp_view() accumulates views instead of replacing them", {
  model <- make_view_test_model()

  mean_view <- view_mean(
    equity,
    target = 0.01
  )

  probability_view <- view_probability(
    inflation > 0.04,
    target = 0.20
  )

  together <- ffp_view(
    model,
    mean_view,
    probability_view
  )

  sequential <- model |>
    ffp_view(mean_view) |>
    ffp_view(probability_view)

  expect_length(together$views, 2)
  expect_length(sequential$views, 2)
  expect_equal(together$views, sequential$views)
})


# Mean-view binding -------------------------------------------------------

test_that("view_mean() resolves scenario variables through the data mask", {
  model <- make_view_test_model()

  equity <- rep(99, 5)

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = equity,
        target = 0.01
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_mean")
  expect_s3_class(bound_view, "ffp_view")
  expect_false("relation" %in% names(bound_view))
  expect_equal(
    bound_view$features$values,
    model$scenarios$equity
  )
})


test_that(".env can explicitly select an external feature", {
  model <- make_view_test_model()
  equity <- seq_len(5)

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = .env$equity,
        target = 0
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    equity
  )
})


test_that(".data supports explicit and programmatic scenario selection", {
  model <- make_view_test_model()
  variable <- "equity"

  direct <- model |>
    ffp_view(
      view_mean(
        x = .data$equity,
        target = 0.01
      )
    )

  programmatic <- model |>
    ffp_view(
      view_mean(
        x = .data[[variable]],
        target = 0.01
      )
    )

  expect_equal(
    direct$views[[1]]$features$values,
    model$scenarios$equity
  )

  expect_equal(
    programmatic$views[[1]]$features$values,
    model$scenarios$equity
  )
})


test_that("view_mean() works through embracing wrappers", {
  model <- make_view_test_model()

  make_mean_view <- function(variable, target) {
    view_mean(
      x = {{ variable }},
      target = target
    )
  }

  bound_model <- model |>
    ffp_view(
      make_mean_view(
        equity,
        target = 0.01
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    model$scenarios$equity
  )
})


test_that("view_mean() supports arbitrary numeric transformations", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = abs(equity) + inflation,
        target = 0.05
      )
    )

  expected <- abs(model$scenarios$equity) + model$scenarios$inflation

  expect_equal(
    bound_model$views[[1]]$features$values,
    expected
  )
})


test_that("bound views are independent of later environment changes", {
  model <- make_view_test_model()
  signal <- c(-0.20, -0.10, 0, 0.20, 0.30)
  original_signal <- signal

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = signal,
        target = 0.05
      )
    )

  signal[] <- 999

  expect_equal(
    bound_model$views[[1]]$features$values,
    original_signal
  )
})


# Scenario-feature inputs -------------------------------------------------

test_that("view features accept numeric vectors", {
  model <- make_view_test_model()
  signal <- seq_len(5)

  bound_model <- model |>
    ffp_view(
      view_mean(signal, target = 0)
    )

  features <- bound_model$views[[1]]$features

  expect_equal(features$values, signal)
  expect_null(features$names)
})


test_that("view features accept matrices and preserve column names", {
  model <- make_view_test_model()

  signals <- cbind(
    value = seq_len(5),
    momentum = seq_len(5) / 10
  )

  bound_model <- model |>
    ffp_view(
      view_mean(
        signals,
        target = c(0, 0)
      )
    )

  features <- bound_model$views[[1]]$features

  expect_equal(features$values, signals)
  expect_identical(features$names, c("value", "momentum"))
})


test_that("view features accept data frames", {
  model <- make_view_test_model()

  signals <- data.frame(
    value = seq_len(5),
    momentum = seq_len(5) / 10
  )

  bound_model <- model |>
    ffp_view(
      view_mean(
        signals,
        target = c(0, 0)
      )
    )

  features <- bound_model$views[[1]]$features

  expect_equal(features$values, signals)
  expect_identical(features$names, c("value", "momentum"))
})


test_that("view features accept tibbles", {
  skip_if_not_installed("tibble")

  model <- make_view_test_model()

  signals <- tibble::tibble(
    value = seq_len(5),
    momentum = seq_len(5) / 10
  )

  bound_model <- model |>
    ffp_view(
      view_mean(
        signals,
        target = c(0, 0)
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    signals
  )
})


test_that("view features accept ts and mts objects by position", {
  model <- make_view_test_model()

  single_series <- stats::ts(seq_len(5))

  single_model <- model |>
    ffp_view(
      view_mean(
        single_series,
        target = 0
      )
    )

  expect_equal(
    single_model$views[[1]]$features$values,
    as.vector(single_series)
  )

  multiple_series <- stats::ts(
    cbind(
      first = seq_len(5),
      second = seq_len(5) / 10
    )
  )

  multiple_model <- model |>
    ffp_view(
      view_mean(
        multiple_series,
        target = c(0, 0)
      )
    )

  expect_equal(
    multiple_model$views[[1]]$features$values,
    as.matrix(multiple_series)
  )

  expect_identical(
    multiple_model$views[[1]]$features$names,
    c("first", "second")
  )
})


test_that("view features accept xts objects by position", {
  skip_if_not_installed("xts")

  model <- make_view_test_model()

  signals <- xts::xts(
    cbind(
      value = seq_len(5),
      momentum = seq_len(5) / 10
    ),
    order.by = as.Date("2020-01-01") + 0:4
  )

  bound_model <- model |>
    ffp_view(
      view_mean(
        signals,
        target = c(0, 0)
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    as.matrix(signals)
  )

  expect_identical(
    bound_model$views[[1]]$features$names,
    c("value", "momentum")
  )
})


test_that("feature observations must match scenario count exactly", {
  model <- make_view_test_model()

  too_short <- seq_len(4)
  too_long <- seq_len(6)

  expect_error(
    ffp_view(
      model,
      view_mean(too_short, target = 0)
    ),
    class = "ffp_error_incompatible_view"
  )

  expect_error(
    ffp_view(
      model,
      view_mean(too_long, target = 0)
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("feature normalization does not transpose or recycle inputs", {
  model <- make_view_test_model()

  transposed <- matrix(
    seq_len(10),
    nrow = 2,
    ncol = 5
  )

  expect_error(
    ffp_view(
      model,
      view_mean(
        transposed,
        target = rep(0, 5)
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("unsupported and higher-dimensional feature inputs are rejected", {
  model <- make_view_test_model()

  list_feature <- as.list(seq_len(5))
  array_feature <- array(seq_len(10), dim = c(5, 1, 2))

  expect_error(
    ffp_view(
      model,
      view_mean(list_feature, target = 0)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_mean(array_feature, target = c(0, 0))
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("data-frame feature inputs reject list-columns", {
  model <- make_view_test_model()

  signals <- data.frame(value = seq_len(5))
  signals$list_column <- I(as.list(seq_len(5)))

  expect_error(
    ffp_view(
      model,
      view_mean(signals, target = c(0, 0))
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("feature names must be complete and unique when present", {
  model <- make_view_test_model()

  partially_named <- matrix(seq_len(10), nrow = 5)
  colnames(partially_named) <- c("signal", "")

  duplicated_names <- matrix(seq_len(10), nrow = 5)
  colnames(duplicated_names) <- c("signal", "signal")

  expect_error(
    ffp_view(
      model,
      view_mean(partially_named, target = c(0, 0))
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_mean(duplicated_names, target = c(0, 0))
    ),
    class = "ffp_error_invalid_view_features"
  )
})


# Mean-feature validation -------------------------------------------------

test_that("view_mean() requires numeric features", {
  model <- make_view_test_model()

  character_feature <- letters[1:5]
  factor_feature <- factor(letters[1:5])

  expect_error(
    ffp_view(
      model,
      view_mean(character_feature, target = 0)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_mean(factor_feature, target = 0)
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_mean() rejects missing and non-finite feature values", {
  model <- make_view_test_model()

  missing_feature <- c(1, 2, NA, 4, 5)
  infinite_feature <- c(1, 2, Inf, 4, 5)

  expect_error(
    ffp_view(
      model,
      view_mean(missing_feature, target = 0)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_mean(infinite_feature, target = 0)
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_mean() requires one target per resolved feature", {
  model <- make_view_test_model()

  panel <- cbind(
    first = seq_len(5),
    second = seq_len(5) / 10
  )

  expect_error(
    ffp_view(
      model,
      view_mean(
        panel,
        target = 0
      )
    ),
    class = "ffp_error_incompatible_view"
  )

  bound_model <- model |>
    ffp_view(
      view_mean(
        panel,
        target = c(0, 0)
      )
    )

  expect_identical(
    bound_model$views[[1]]$target,
    c(0, 0)
  )
})


# view_probability(): specification ---------------------------------------

test_that("view_probability() creates a probability-view specification", {
  view <- view_probability(
    x = equity < 0,
    target = 0.30
  )

  expect_s3_class(view, "ffp_view_probability_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "probability")
  expect_identical(view$target, 0.30)
  expect_false("relation" %in% names(view))
  expect_null(view$parameters$given)
})


test_that("view_probability() stores a conditioning expression when supplied", {
  view <- view_probability(
    x = equity < 0,
    target = 0.70,
    given = recession
  )

  expect_s3_class(view, "ffp_view_probability_spec")
  expect_true(inherits(view$parameters$given, "quosure"))
  expect_false("relation" %in% names(view))
})


test_that("view_probability() requires x and target", {
  expect_error(
    view_probability(target = 0.20),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_probability(x = recession),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_probability() validates probability targets eagerly", {
  expect_error(
    view_probability(recession, target = -0.01),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_probability(recession, target = 1.01),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_probability(recession, target = NA_real_),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_probability(recession, target = "20%"),
    class = "ffp_error_invalid_view_target"
  )

  expect_s3_class(
    view_probability(recession, target = 0),
    "ffp_view_probability_spec"
  )

  expect_s3_class(
    view_probability(recession, target = 1),
    "ffp_view_probability_spec"
  )
})


# Probability-view binding ------------------------------------------------

test_that("view_probability() resolves a marginal event", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_probability(
        x = inflation > 0.04,
        target = 0.20
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_probability")
  expect_false("relation" %in% names(bound_view))
  expect_equal(
    bound_view$features$values,
    model$scenarios$inflation > 0.04
  )
  expect_null(bound_view$parameters$given)
})


test_that("view_probability() uses ordinary R logic for compound events", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_probability(
        x = equity < 0 & recession,
        target = 0.40
      )
    )

  expected <- model$scenarios$equity < 0 & model$scenarios$recession

  expect_equal(
    bound_model$views[[1]]$features$values,
    expected
  )
})


test_that("view_probability() supports multiple explicitly targeted events", {
  model <- make_view_test_model()

  events <- cbind(
    loss = model$scenarios$equity < 0,
    high_inflation = model$scenarios$inflation > 0.04
  )

  bound_model <- model |>
    ffp_view(
      view_probability(
        events,
        target = c(0.30, 0.20)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_equal(bound_view$features$values, events)
  expect_identical(bound_view$features$names, c("loss", "high_inflation"))
  expect_identical(bound_view$target, c(0.30, 0.20))
  expect_false("relation" %in% names(bound_view))
})


test_that("view_probability() never recycles a scalar target across events", {
  model <- make_view_test_model()

  events <- cbind(
    loss = model$scenarios$equity < 0,
    high_inflation = model$scenarios$inflation > 0.04
  )

  expect_error(
    ffp_view(
      model,
      view_probability(
        events,
        target = 0.20
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("view_probability() requires logical event features", {
  model <- make_view_test_model()

  numeric_indicator <- c(1, 1, 0, 0, 0)

  expect_error(
    ffp_view(
      model,
      view_probability(
        numeric_indicator,
        target = 0.40
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  bound_model <- model |>
    ffp_view(
      view_probability(
        numeric_indicator == 1,
        target = 0.40
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    numeric_indicator == 1
  )
})


test_that("view_probability() rejects missing event values", {
  model <- make_view_test_model()
  event <- c(TRUE, FALSE, NA, TRUE, FALSE)

  expect_error(
    ffp_view(
      model,
      view_probability(event, target = 0.40)
    ),
    class = "ffp_error_invalid_view_features"
  )
})


# Conditional probabilities ----------------------------------------------

test_that("conditional probability binds one conditioning event", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = recession
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_probability")
  expect_equal(
    bound_view$parameters$given$values,
    model$scenarios$recession
  )
  expect_identical(
    bound_view$metadata$given_expression,
    "recession"
  )
})


test_that("one given event can condition multiple target events", {
  model <- make_view_test_model()

  events <- cbind(
    loss = model$scenarios$equity < 0,
    high_inflation = model$scenarios$inflation > 0.025
  )

  bound_model <- model |>
    ffp_view(
      view_probability(
        x = events,
        target = c(0.70, 0.40),
        given = recession
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_equal(NCOL(bound_view$features$values), 2)
  expect_equal(NCOL(as.matrix(bound_view$parameters$given$values)), 1)
})


test_that("given must define exactly one event", {
  model <- make_view_test_model()

  conditioning_events <- cbind(
    recession = model$scenarios$recession,
    high_inflation = model$scenarios$inflation > 0.04
  )

  expect_error(
    ffp_view(
      model,
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = conditioning_events
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("given must be logical and match scenario count", {
  model <- make_view_test_model()

  numeric_given <- c(1, 1, 0, 0, 0)
  short_given <- c(TRUE, FALSE, TRUE, FALSE)

  expect_error(
    ffp_view(
      model,
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = numeric_given
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = short_given
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("given rejects missing event values", {
  model <- make_view_test_model()
  missing_given <- c(TRUE, TRUE, NA, FALSE, FALSE)

  expect_error(
    ffp_view(
      model,
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = missing_given
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("given must have positive probability under the model prior", {
  scenarios <- data.frame(
    event = c(TRUE, FALSE, TRUE, FALSE),
    conditioning = c(TRUE, TRUE, FALSE, FALSE)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(c(0, 0, 0.5, 0.5))
    )

  expect_error(
    ffp_view(
      model,
      view_probability(
        x = event,
        target = 0.50,
        given = conditioning
      )
    ),
    class = "ffp_error_zero_conditioning_probability"
  )
})


test_that("positive conditioning support is evaluated using the current prior", {
  scenarios <- data.frame(
    event = c(TRUE, FALSE, TRUE, FALSE),
    conditioning = c(TRUE, TRUE, FALSE, FALSE)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(c(0.25, 0.25, 0.25, 0.25))
    ) |>
    ffp_view(
      view_probability(
        x = event,
        target = 0.50,
        given = conditioning
      )
    )

  expect_length(model$views, 1)
  expect_s3_class(model$views[[1]], "ffp_view_probability")
})


# view_quantile(): specification ------------------------------------------

test_that("view_quantile() creates a quantile-view specification", {
  view <- view_quantile(
    x = equity,
    target = -0.08,
    level = 0.05
  )

  expect_s3_class(view, "ffp_view_quantile_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "quantile")
  expect_identical(view$target, -0.08)
  expect_identical(view$parameters$level, 0.05)
  expect_false("relation" %in% names(view))
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_quantile() follows the x, target, level argument order", {
  view <- view_quantile(
    equity,
    -0.08,
    0.05
  )

  expect_identical(view$target, -0.08)
  expect_identical(view$parameters$level, 0.05)
})


test_that("view_quantile() does not evaluate x at construction time", {
  view <- view_quantile(
    x = stop("x should only be evaluated during binding"),
    target = 0,
    level = 0.50
  )

  expect_s3_class(view, "ffp_view_quantile_spec")

  model <- make_view_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_quantile() requires x, target, and level", {
  expect_error(
    view_quantile(target = 0, level = 0.50),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_quantile(x = equity, level = 0.50),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_quantile(x = equity, target = 0),
    class = "ffp_error_invalid_view_level"
  )
})


test_that("view_quantile() validates numeric targets eagerly", {
  expect_error(
    view_quantile(equity, target = "-0.08", level = 0.05),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_quantile(equity, target = matrix(-0.08), level = 0.05),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_quantile(equity, target = numeric(), level = 0.05),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_quantile(equity, target = c(-0.08, NA_real_), level = 0.05),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_quantile(equity, target = Inf, level = 0.05),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_quantile() validates quantile levels eagerly", {
  expect_error(
    view_quantile(equity, target = -0.08, level = "0.05"),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = matrix(0.05)),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = numeric()),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = NA_real_),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = Inf),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = 0),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = 1),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = -0.01),
    class = "ffp_error_invalid_view_level"
  )

  expect_error(
    view_quantile(equity, target = -0.08, level = 1.01),
    class = "ffp_error_invalid_view_level"
  )
})


test_that("a scalar quantile level applies to every explicit target", {
  view <- view_quantile(
    x = ret,
    target = c(-0.08, -0.07, -0.09),
    level = 0.05
  )

  expect_identical(
    view$parameters$level,
    rep(0.05, 3)
  )
})


test_that("quantile levels may be supplied one per target", {
  view <- view_quantile(
    x = ret,
    target = c(-0.08, -0.05, 0.07),
    level = c(0.05, 0.10, 0.95)
  )

  expect_identical(
    view$parameters$level,
    c(0.05, 0.10, 0.95)
  )

  expect_error(
    view_quantile(
      x = ret,
      target = c(-0.08, -0.05, 0.07),
      level = c(0.05, 0.10)
    ),
    class = "ffp_error_invalid_view_level"
  )
})



test_that("view_quantile() supports .data selection", {
  model <- make_view_test_model()
  variable <- "equity"

  direct <- model |>
    ffp_view(
      view_quantile(
        x = .data$equity,
        target = -0.05,
        level = 0.05
      )
    )

  programmatic <- model |>
    ffp_view(
      view_quantile(
        x = .data[[variable]],
        target = -0.05,
        level = 0.05
      )
    )

  expect_equal(
    direct$views[[1]]$features$values,
    model$scenarios$equity
  )

  expect_equal(
    programmatic$views[[1]]$features$values,
    model$scenarios$equity
  )
})


test_that("view_quantile() works through embracing wrappers", {
  model <- make_view_test_model()

  make_quantile_view <- function(variable, target, level) {
    view_quantile(
      x = {{ variable }},
      target = target,
      level = level
    )
  }

  bound_model <- model |>
    ffp_view(
      make_quantile_view(
        equity,
        target = -0.05,
        level = 0.05
      )
    )

  expect_equal(
    bound_model$views[[1]]$features$values,
    model$scenarios$equity
  )
})


test_that("view_quantile() preserves per-feature level alignment after binding", {
  model <- make_view_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(-0.05, 0.045),
        level = c(0.05, 0.95)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_identical(
    bound_view$features$names,
    c("equity", "inflation")
  )
  expect_identical(
    bound_view$target,
    c(-0.05, 0.045)
  )
  expect_identical(
    bound_view$parameters$level,
    c(0.05, 0.95)
  )
})


test_that("heterogeneous quantile levels still create two constraints per target", {
  model <- make_view_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(-0.05, 0.045),
        level = c(0.05, 0.95)
      )
    )

  expect_identical(
    view_constraint_count(bound_model$views[[1]]),
    4L
  )
})


# Quantile-view binding ---------------------------------------------------

test_that("view_quantile() resolves scenario variables through the data mask", {
  model <- make_view_test_model()
  equity <- rep(99, 5)

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = equity,
        target = -0.05,
        level = 0.05
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_quantile")
  expect_s3_class(bound_view, "ffp_view")
  expect_false("relation" %in% names(bound_view))
  expect_equal(
    bound_view$features$values,
    model$scenarios$equity
  )
  expect_identical(bound_view$parameters$level, 0.05)
})


test_that("view_quantile() supports transformations and external features", {
  model <- make_view_test_model()
  signal <- c(-2, -1, 0, 1, 2)

  transformed <- model |>
    ffp_view(
      view_quantile(
        x = abs(equity),
        target = 0.05,
        level = 0.90
      )
    )

  external <- model |>
    ffp_view(
      view_quantile(
        x = .env$signal,
        target = 0,
        level = 0.50
      )
    )

  expect_equal(
    transformed$views[[1]]$features$values,
    abs(model$scenarios$equity)
  )
  expect_equal(
    external$views[[1]]$features$values,
    signal
  )
})


test_that("view_quantile() accepts numeric panels and preserves feature names", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(-0.05, 0.045),
        level = 0.05
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_equal(bound_view$features$values, panel)
  expect_identical(bound_view$features$names, c("equity", "inflation"))
  expect_identical(bound_view$parameters$level, c(0.05, 0.05))
})


test_that("view_quantile() requires numeric features", {
  model <- make_view_test_model()
  regimes <- c("bear", "bear", "flat", "bull", "bull")
  logical_feature <- model$scenarios$equity < 0

  expect_error(
    ffp_view(
      model,
      view_quantile(
        x = regimes,
        target = 0,
        level = 0.50
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_quantile(
        x = logical_feature,
        target = 0,
        level = 0.50
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_quantile() rejects missing and non-finite feature values", {
  model <- make_view_test_model()
  missing_feature <- c(-0.08, -0.03, NA, 0.04, 0.06)
  infinite_feature <- c(-0.08, -0.03, 0.01, Inf, 0.06)

  expect_error(
    ffp_view(
      model,
      view_quantile(
        x = missing_feature,
        target = 0,
        level = 0.50
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_quantile(
        x = infinite_feature,
        target = 0,
        level = 0.50
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_quantile() requires one target per resolved feature", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  expect_error(
    ffp_view(
      model,
      view_quantile(
        x = panel,
        target = -0.05,
        level = 0.05
      )
    ),
    class = "ffp_error_incompatible_view"
  )

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(-0.05, 0.045),
        level = 0.05
      )
    )

  expect_length(bound_model$views, 1)
  expect_length(bound_model$views[[1]]$target, 2)
})


test_that("bound quantile views are independent of later environment changes", {
  model <- make_view_test_model()
  signal <- c(-2, -1, 0, 1, 2)

  bound_model <- model |>
    ffp_view(
      view_quantile(
        x = signal,
        target = 0,
        level = 0.50
      )
    )

  expected <- signal
  signal <- rep(100, 5)

  expect_equal(
    bound_model$views[[1]]$features$values,
    expected
  )
})



# view_median(): specification --------------------------------------------

test_that("view_median() creates a median-view specification", {
  view <- view_median(
    x = equity,
    target = 0.01
  )

  expect_s3_class(view, "ffp_view_median_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "median")
  expect_identical(view$target, 0.01)
  expect_identical(view$parameters, list())
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_median() does not evaluate x at construction time", {
  view <- view_median(
    x = stop("x should only be evaluated during binding"),
    target = 0
  )

  expect_s3_class(view, "ffp_view_median_spec")

  model <- make_view_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_median() requires x and target", {
  expect_error(
    view_median(target = 0),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_median(x = equity),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_median() validates numeric targets eagerly", {
  expect_error(
    view_median(equity, target = "0.01"),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_median(equity, target = matrix(0.01)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_median(equity, target = numeric()),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_median(equity, target = c(0.01, NA_real_)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_median(equity, target = Inf),
    class = "ffp_error_invalid_view_target"
  )
})


# Median-view binding -----------------------------------------------------

test_that("view_median() resolves scenario variables through the data mask", {
  model <- make_view_test_model()
  equity <- rep(99, 5)

  bound_model <- model |>
    ffp_view(
      view_median(
        x = equity,
        target = 0.01
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_median")
  expect_s3_class(bound_view, "ffp_view")
  expect_identical(bound_view$method, "median")
  expect_identical(bound_view$target, 0.01)
  expect_equal(
    bound_view$features$values,
    model$scenarios$equity
  )
})


test_that("view_median() supports transformations and external features", {
  model <- make_view_test_model()
  signal <- c(-2, -1, 0, 1, 2)

  transformed <- model |>
    ffp_view(
      view_median(
        x = abs(equity),
        target = 0.03
      )
    )

  external <- model |>
    ffp_view(
      view_median(
        x = .env$signal,
        target = 0
      )
    )

  expect_equal(
    transformed$views[[1]]$features$values,
    abs(model$scenarios$equity)
  )

  expect_equal(
    external$views[[1]]$features$values,
    signal
  )
})


test_that("view_median() accepts numeric panels and preserves feature names", {
  model <- make_view_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_median(
        x = panel,
        target = c(0.01, 0.035)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_equal(bound_view$features$values, panel)
  expect_identical(bound_view$features$names, c("equity", "inflation"))
  expect_identical(bound_view$target, c(0.01, 0.035))
})


test_that("view_median() requires one target per resolved feature", {
  model <- make_view_test_model()

  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  expect_error(
    ffp_view(
      model,
      view_median(
        x = panel,
        target = 0.01
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("view_median() requires numeric features", {
  model <- make_view_test_model()
  regimes <- c("bear", "bear", "flat", "bull", "bull")
  logical_feature <- model$scenarios$equity < 0

  expect_error(
    ffp_view(
      model,
      view_median(
        x = regimes,
        target = 0
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_median(
        x = logical_feature,
        target = 0
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_median() rejects missing and non-finite feature values", {
  model <- make_view_test_model()

  missing_feature <- c(-0.08, -0.03, NA, 0.04, 0.06)
  infinite_feature <- c(-0.08, -0.03, 0.01, Inf, 0.06)

  expect_error(
    ffp_view(
      model,
      view_median(
        x = missing_feature,
        target = 0
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_median(
        x = infinite_feature,
        target = 0
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("bound median views are independent of later environment changes", {
  model <- make_view_test_model()
  signal <- c(-2, -1, 0, 1, 2)
  original_signal <- signal

  bound_model <- model |>
    ffp_view(
      view_median(
        x = signal,
        target = 0
      )
    )

  signal[] <- 999

  expect_equal(
    bound_model$views[[1]]$features$values,
    original_signal
  )
})


# view_rank(): specification ----------------------------------------------

test_that("view_rank() creates a ranking-view specification", {
  view <- view_rank(
    x = returns,
    order = c("DAX", "FTSE", "CAC")
  )

  expect_s3_class(view, "ffp_view_rank_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "rank")
  expect_null(view$target)
  expect_identical(
    view$parameters$order,
    c("DAX", "FTSE", "CAC")
  )
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_rank() accepts an explicit numeric ranking order", {
  view <- view_rank(
    x = returns,
    order = c(1, 4, 2)
  )

  expect_identical(
    view$parameters$order,
    c(1L, 4L, 2L)
  )
})


test_that("view_rank() does not evaluate x at construction time", {
  view <- view_rank(
    x = stop("x should only be evaluated during binding"),
    order = c(1, 2)
  )

  expect_s3_class(view, "ffp_view_rank_spec")

  model <- make_view_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_rank() requires x and an explicit order", {
  expect_error(
    view_rank(order = c(1, 2)),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_rank(x = returns),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() validates character order eagerly", {
  expect_error(
    view_rank(returns, order = "DAX"),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c("DAX", NA_character_)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c("DAX", "")),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c("DAX", "DAX")),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() validates numeric order eagerly", {
  expect_error(
    view_rank(returns, order = 1),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c(1, NA_real_)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c(1, Inf)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c(0, 1)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c(1, 2.5)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = c(1, 1)),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() rejects unsupported order types", {
  expect_error(
    view_rank(returns, order = c(TRUE, FALSE)),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = matrix(c(1, 2))),
    class = "ffp_error_invalid_view_order"
  )

  expect_error(
    view_rank(returns, order = list("DAX", "FTSE")),
    class = "ffp_error_invalid_view_order"
  )
})



test_that("view_rank() uses numeric positions with unnamed panels", {
  model <- make_view_test_model()

  returns <- matrix(
    seq_len(20) / 100,
    nrow = 5,
    ncol = 4
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c(4, 1, 3)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_null(bound_view$features$names)
  expect_equal(
    bound_view$features$values,
    returns[, c(4, 1, 3), drop = FALSE]
  )
})


test_that("character and numeric rank orders resolve to the same features", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01),
    SP500 = c(0.02, -0.01, 0.01, 0.00, 0.03)
  )

  by_name <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("SP500", "DAX", "FTSE")
      )
    )

  by_position <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c(4, 1, 2)
      )
    )

  expect_equal(
    by_name$views[[1]]$features$values,
    by_position$views[[1]]$features$values
  )

  expect_identical(
    by_name$views[[1]]$features$names,
    by_position$views[[1]]$features$names
  )
})


test_that("view_rank() preserves the declared order instead of sorting features", {
  model <- make_view_test_model()

  returns <- cbind(
    low_mean = rep(-0.10, 5),
    high_mean = rep(0.10, 5),
    middle_mean = rep(0, 5)
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("low_mean", "high_mean", "middle_mean")
      )
    )

  expect_identical(
    bound_model$views[[1]]$features$names,
    c("low_mean", "high_mean", "middle_mean")
  )
})


test_that("view_rank() supports ranking transformed scenario features", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = cbind(
          spread = equity - inflation,
          equity = equity,
          inflation = inflation
        ),
        order = c("spread", "equity")
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_identical(
    bound_view$features$names,
    c("spread", "equity")
  )

  expect_equal(
    bound_view$features$values[, "spread"],
    model$scenarios$equity - model$scenarios$inflation
  )

  expect_equal(
    bound_view$features$values[, "equity"],
    model$scenarios$equity
  )
})


test_that("ranking exactly two features creates one constraint", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = seq_len(5) / 100,
    FTSE = seq_len(5) / 200,
    CAC = seq_len(5) / 300
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("DAX", "CAC")
      )
    )

  expect_identical(
    view_constraint_count(bound_model$views[[1]]),
    1L
  )
})


test_that("numeric ranking specification printing reports positions", {
  view <- view_rank(
    x = returns,
    order = c(1, 4, 2)
  )

  output <- capture.output(print(view))

  expect_true(
    any(grepl("Order:       1 >= 4 >= 2", output, fixed = TRUE))
  )
})


# Ranking-view binding ----------------------------------------------------

test_that("view_rank() selects and reorders features by name", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01),
    SP500 = c(0.02, -0.01, 0.01, 0.00, 0.03)
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("SP500", "DAX", "FTSE")
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_rank")
  expect_s3_class(bound_view, "ffp_view")
  expect_null(bound_view$target)
  expect_identical(
    bound_view$features$names,
    c("SP500", "DAX", "FTSE")
  )
  expect_equal(
    bound_view$features$values,
    returns[, c("SP500", "DAX", "FTSE"), drop = FALSE]
  )
})


test_that("view_rank() selects and reorders features by position", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01),
    SP500 = c(0.02, -0.01, 0.01, 0.00, 0.03)
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c(1, 4, 2)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_identical(
    bound_view$features$names,
    c("DAX", "SP500", "FTSE")
  )
  expect_equal(
    bound_view$features$values,
    returns[, c(1, 4, 2), drop = FALSE]
  )
})


test_that("view_rank() allows an explicitly selected subset", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01),
    SP500 = c(0.02, -0.01, 0.01, 0.00, 0.03)
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("DAX", "CAC")
      )
    )

  expect_identical(
    bound_model$views[[1]]$features$names,
    c("DAX", "CAC")
  )
})


test_that("character rank order requires named resolved features", {
  model <- make_view_test_model()

  returns <- matrix(
    seq_len(20) / 100,
    nrow = 5,
    ncol = 4
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = returns,
        order = c("DAX", "FTSE")
      )
    ),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() rejects unknown feature names", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = seq_len(5) / 100,
    FTSE = seq_len(5) / 200,
    CAC = seq_len(5) / 300
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = returns,
        order = c("DAX", "SP500")
      )
    ),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() rejects positions outside the resolved panel", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = seq_len(5) / 100,
    FTSE = seq_len(5) / 200,
    CAC = seq_len(5) / 300
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = returns,
        order = c(1, 4)
      )
    ),
    class = "ffp_error_invalid_view_order"
  )
})


test_that("view_rank() validates only the explicitly selected features", {
  model <- make_view_test_model()

  panel <- data.frame(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    regime = c("bear", "bear", "flat", "bull", "bull"),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00)
  )

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = panel,
        order = c("DAX", "FTSE")
      )
    )

  expect_identical(
    bound_model$views[[1]]$features$names,
    c("DAX", "FTSE")
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = panel,
        order = c("DAX", "regime")
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_rank() rejects missing and non-finite selected features", {
  model <- make_view_test_model()

  missing_panel <- cbind(
    DAX = c(0.01, -0.02, NA, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00)
  )

  infinite_panel <- cbind(
    DAX = c(0.01, -0.02, Inf, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00)
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = missing_panel,
        order = c("DAX", "FTSE")
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_rank(
        x = infinite_panel,
        order = c("DAX", "FTSE")
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("bound ranking views are independent of environment changes", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01)
  )

  expected <- returns[, c("CAC", "DAX"), drop = FALSE]

  bound_model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("CAC", "DAX")
      )
    )

  returns[,] <- 999

  expect_equal(
    bound_model$views[[1]]$features$values,
    expected
  )
})


# Constraint counting -----------------------------------------------------

test_that("views report their mathematical constraint counts", {
  model <- make_view_test_model()

  numeric_panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  event_panel <- cbind(
    loss = model$scenarios$equity < 0,
    high_inflation = model$scenarios$inflation > 0.04
  )

  rank_panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation,
    spread = model$scenarios$equity - model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_mean(
        x = numeric_panel,
        target = c(0.01, 0.03)
      ),
      view_probability(
        x = event_panel,
        target = c(0.40, 0.20)
      ),
      view_quantile(
        x = numeric_panel,
        target = c(-0.05, 0.045),
        level = 0.05
      ),
      view_median(
        x = numeric_panel,
        target = c(0.01, 0.035)
      ),
      view_rank(
        x = rank_panel,
        order = c("spread", "equity", "inflation")
      ),
      view_volatility(
        x = numeric_panel,
        target = c(0.15, 0.03)
      )
    )

  expect_identical(view_constraint_count(model$views[[1]]), 2L)
  expect_identical(view_constraint_count(model$views[[2]]), 2L)
  expect_identical(view_constraint_count(model$views[[3]]), 4L)
  expect_identical(view_constraint_count(model$views[[4]]), 4L)
  expect_identical(view_constraint_count(model$views[[5]]), 2L)
  expect_identical(view_constraint_count(model$views[[6]]), 2L)
})


# Quantile metadata and printing -----------------------------------------

test_that("bound quantile views preserve compact provenance metadata", {
  model <- make_view_test_model() |>
    ffp_view(
      view_quantile(
        x = equity - inflation,
        target = -0.06,
        level = 0.05
      )
    )

  expect_identical(
    model$views[[1]]$metadata$expression,
    "equity - inflation"
  )
})


test_that("quantile specification printing reports its level", {
  view <- view_quantile(
    x = equity,
    target = -0.08,
    level = 0.05
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("<ffp_view_spec>", output, fixed = TRUE)))
  expect_true(any(grepl("Method:      quantile", output, fixed = TRUE)))
  expect_true(any(grepl("Level:       0.05", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})


test_that("bound quantile printing reports two constraints per feature", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  model <- model |>
    ffp_view(
      view_quantile(
        x = panel,
        target = c(-0.05, 0.045),
        level = 0.05
      )
    )

  output <- capture.output(print(model$views[[1]]))

  expect_true(any(grepl("<ffp_view_quantile>", output, fixed = TRUE)))
  expect_true(any(grepl("Features:    2", output, fixed = TRUE)))
  expect_true(any(grepl("Constraints: 4", output, fixed = TRUE)))
  expect_true(any(grepl("Level:       0.05", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})


test_that("mixed quantile levels print compactly", {
  view <- view_quantile(
    x = ret,
    target = c(-0.08, 0.07),
    level = c(0.05, 0.95)
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("Level:       mixed", output, fixed = TRUE)))
})


# Bound-view metadata and printing ----------------------------------------

test_that("bound views preserve compact provenance metadata", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = equity - inflation,
        target = 0
      ),
      view_probability(
        x = equity < 0,
        target = 0.70,
        given = recession
      )
    )

  expect_identical(
    bound_model$views[[1]]$metadata$expression,
    "equity - inflation"
  )

  expect_identical(
    bound_model$views[[2]]$metadata$expression,
    "equity < 0"
  )

  expect_identical(
    bound_model$views[[2]]$metadata$given_expression,
    "recession"
  )
})


test_that("view specification printing is compact", {
  view <- view_probability(
    x = equity < 0,
    target = 0.70,
    given = recession
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("<ffp_view_spec>", output, fixed = TRUE)))
  expect_true(any(grepl("Method:      probability", output, fixed = TRUE)))
  expect_true(any(grepl("Targets:     1", output, fixed = TRUE)))
  expect_true(any(grepl("Conditional: yes", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})


test_that("bound-view printing is compact", {
  model <- make_view_test_model() |>
    ffp_view(
      view_mean(
        equity,
        target = 0.01
      )
    )

  output <- capture.output(
    print(model$views[[1]])
  )

  expect_true(any(grepl("<ffp_view_mean>", output, fixed = TRUE)))
  expect_true(any(grepl("Features:    1", output, fixed = TRUE)))
  expect_true(any(grepl("Constraints: 1", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})


test_that("ffp_model printing reports accumulated view count", {
  model <- make_view_test_model() |>
    ffp_view(
      view_mean(
        equity,
        target = 0.01
      ),
      view_probability(
        inflation > 0.04,
        target = 0.20
      )
    )

  output <- capture.output(print(model))

  expect_true(any(grepl("2 view(s)", output, fixed = TRUE)))
})


# Compact expression labels ----------------------------------------------

test_that("expression labels remain compact without affecting values", {
  model <- make_view_test_model()

  bound_model <- model |>
    ffp_view(
      view_mean(
        x = equity + inflation + equity + inflation +
          equity + inflation + equity + inflation +
          equity + inflation,
        target = 0
      )
    )

  label <- bound_model$views[[1]]$metadata$expression
  expected <- 5 * (model$scenarios$equity + model$scenarios$inflation)

  expect_true(nzchar(label))
  expect_true(nchar(label, type = "width") <= 60)
  expect_equal(bound_model$views[[1]]$features$values, expected)
})



# Median and ranking printing ---------------------------------------------

test_that("median-view specification printing is compact", {
  view <- view_median(
    x = equity,
    target = 0.01
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("<ffp_view_spec>", output, fixed = TRUE)))
  expect_true(any(grepl("Method:      median", output, fixed = TRUE)))
  expect_true(any(grepl("Targets:     1", output, fixed = TRUE)))
})


test_that("bound median printing reports two constraints per feature", {
  model <- make_view_test_model()

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

  output <- capture.output(print(model$views[[1]]))

  expect_true(any(grepl("<ffp_view_median>", output, fixed = TRUE)))
  expect_true(any(grepl("Features:    2", output, fixed = TRUE)))
  expect_true(any(grepl("Constraints: 4", output, fixed = TRUE)))
})


test_that("ranking-view specification printing reports explicit order", {
  view <- view_rank(
    x = returns,
    order = c("DAX", "FTSE", "CAC")
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("<ffp_view_spec>", output, fixed = TRUE)))
  expect_true(any(grepl("Method:      rank", output, fixed = TRUE)))
  expect_true(any(grepl("Order:       DAX >= FTSE >= CAC", output, fixed = TRUE)))
  expect_false(any(grepl("Targets:", output, fixed = TRUE)))
})


test_that("bound ranking printing reports selected names and constraints", {
  model <- make_view_test_model()

  returns <- cbind(
    DAX = c(0.01, -0.02, 0.03, 0.02, 0.01),
    FTSE = c(0.00, -0.01, 0.02, 0.01, 0.00),
    CAC = c(-0.01, 0.00, 0.01, 0.02, 0.01)
  )

  model <- model |>
    ffp_view(
      view_rank(
        x = returns,
        order = c("CAC", "DAX", "FTSE")
      )
    )

  output <- capture.output(print(model$views[[1]]))

  expect_true(any(grepl("<ffp_view_rank>", output, fixed = TRUE)))
  expect_true(any(grepl("Features:    3", output, fixed = TRUE)))
  expect_true(any(grepl("Constraints: 2", output, fixed = TRUE)))
  expect_true(any(grepl("Order:       CAC >= DAX >= FTSE", output, fixed = TRUE)))
  expect_false(any(grepl("Targets:", output, fixed = TRUE)))
})


# view_volatility(): specification ---------------------------------------

test_that("view_volatility() creates a volatility-view specification", {
  view <- view_volatility(
    x = equity,
    target = 0.15
  )

  expect_s3_class(view, "ffp_view_volatility_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "volatility")
  expect_identical(view$target, 0.15)
  expect_false("relation" %in% names(view))
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_volatility() does not evaluate x at construction time", {
  view <- view_volatility(
    x = stop("x should only be evaluated during binding"),
    target = 0.15
  )

  expect_s3_class(view, "ffp_view_volatility_spec")

  model <- make_view_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_volatility() requires x and target", {
  expect_error(
    view_volatility(target = 0.15),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_volatility(x = equity),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_volatility() validates non-negative numeric targets eagerly", {
  expect_error(
    view_volatility(equity, target = "0.15"),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_volatility(equity, target = matrix(0.15)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_volatility(equity, target = numeric()),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_volatility(equity, target = c(0.15, NA_real_)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_volatility(equity, target = Inf),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_volatility(equity, target = -0.01),
    class = "ffp_error_invalid_view_target"
  )

  expect_s3_class(
    view_volatility(equity, target = 0),
    "ffp_view_volatility_spec"
  )
})


# Volatility-view binding -------------------------------------------------

test_that("view_volatility() resolves scenario variables through the data mask", {
  model <- make_view_test_model()
  equity <- rep(99, 5)

  bound_model <- model |>
    ffp_view(
      view_volatility(
        x = equity,
        target = 0.15
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_volatility")
  expect_s3_class(bound_view, "ffp_view")
  expect_false("relation" %in% names(bound_view))
  expect_equal(
    bound_view$features$values,
    model$scenarios$equity
  )
  expect_identical(bound_view$target, 0.15)
})


test_that("view_volatility() supports transformations and external features", {
  model <- make_view_test_model()
  signal <- c(-0.20, -0.10, 0, 0.20, 0.30)

  transformed <- model |>
    ffp_view(
      view_volatility(
        x = abs(equity) + inflation,
        target = 0.08
      )
    )

  external <- model |>
    ffp_view(
      view_volatility(
        x = .env$signal,
        target = 0.12
      )
    )

  expected <- abs(model$scenarios$equity) + model$scenarios$inflation

  expect_equal(transformed$views[[1]]$features$values, expected)
  expect_equal(external$views[[1]]$features$values, signal)
})


test_that("view_volatility() accepts numeric panels and preserves names", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_volatility(
        x = panel,
        target = c(0.15, 0.03)
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_equal(bound_view$features$values, panel)
  expect_identical(bound_view$features$names, c("equity", "inflation"))
  expect_identical(bound_view$target, c(0.15, 0.03))
  expect_false("relation" %in% names(bound_view))
})


test_that("view_volatility() requires one target per resolved feature", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  expect_error(
    ffp_view(
      model,
      view_volatility(
        x = panel,
        target = 0.15
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("view_volatility() requires numeric features", {
  model <- make_view_test_model()
  regimes <- c("bear", "bear", "flat", "bull", "bull")
  logical_feature <- model$scenarios$equity < 0

  expect_error(
    ffp_view(
      model,
      view_volatility(
        x = regimes,
        target = 0.15
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_volatility(
        x = logical_feature,
        target = 0.15
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_volatility() rejects missing and non-finite feature values", {
  model <- make_view_test_model()
  missing_feature <- c(-0.08, -0.03, NA, 0.04, 0.06)
  infinite_feature <- c(-0.08, -0.03, 0.01, Inf, 0.06)

  expect_error(
    ffp_view(
      model,
      view_volatility(
        x = missing_feature,
        target = 0.15
      )
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_volatility(
        x = infinite_feature,
        target = 0.15
      )
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("bound volatility views are independent of environment changes", {
  model <- make_view_test_model()
  signal <- c(-0.20, -0.10, 0, 0.20, 0.30)
  original_signal <- signal

  bound_model <- model |>
    ffp_view(
      view_volatility(
        x = signal,
        target = 0.12
      )
    )

  signal[] <- 999

  expect_equal(
    bound_model$views[[1]]$features$values,
    original_signal
  )
})


# Volatility printing -----------------------------------------------------

test_that("volatility-view specification printing is compact", {
  view <- view_volatility(
    x = equity,
    target = 0.15
  )

  output <- capture.output(print(view))

  expect_true(any(grepl("<ffp_view_spec>", output, fixed = TRUE)))
  expect_true(any(grepl("Method:      volatility", output, fixed = TRUE)))
  expect_true(any(grepl("Targets:     1", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})


test_that("bound volatility-view printing reports one constraint per feature", {
  model <- make_view_test_model()
  panel <- cbind(
    equity = model$scenarios$equity,
    inflation = model$scenarios$inflation
  )

  bound_model <- model |>
    ffp_view(
      view_volatility(
        x = panel,
        target = c(0.15, 0.03)
      )
    )

  output <- capture.output(print(bound_model$views[[1]]))

  expect_true(any(grepl("<ffp_view_volatility>", output, fixed = TRUE)))
  expect_true(any(grepl("Features:    2", output, fixed = TRUE)))
  expect_true(any(grepl("Constraints: 2", output, fixed = TRUE)))
  expect_false(any(grepl("Relation:", output, fixed = TRUE)))
})

# Helpers -----------------------------------------------------------------

make_second_moment_test_model <- function(prior = prior_uniform()) {
  scenarios <- data.frame(
    DAX = c(-0.08, -0.03, 0.01, 0.04, 0.06),
    FTSE = c(-0.06, -0.02, 0.00, 0.03, 0.05),
    CAC = c(-0.07, -0.01, 0.02, 0.03, 0.04)
  )

  ffp_model(scenarios) |>
    ffp_prior(prior)
}


make_covariance_target <- function() {
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


make_correlation_target <- function() {
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


# Public API ---------------------------------------------------------------

test_that("second-moment views expose the intended public API", {
  expect_identical(
    names(formals(view_covariance)),
    c("x", "target")
  )

  expect_identical(
    names(formals(view_correlation)),
    c("x", "target")
  )
})


# view_covariance(): specification ----------------------------------------

test_that("view_covariance() creates a covariance-view specification", {
  target <- make_covariance_target()

  view <- view_covariance(
    x = returns,
    target = target
  )

  expect_s3_class(view, "ffp_view_covariance_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "covariance")
  expect_identical(view$target, target)
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_covariance() does not evaluate x at construction time", {
  target <- diag(c(0.04, 0.01))

  view <- view_covariance(
    x = stop("x should only be evaluated during binding"),
    target = target
  )

  expect_s3_class(view, "ffp_view_covariance_spec")

  model <- make_second_moment_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_covariance() requires x and target", {
  target <- diag(c(0.04, 0.01))

  expect_error(
    view_covariance(target = target),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_covariance(x = returns),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_covariance() requires a numeric square matrix target", {
  expect_error(
    view_covariance(returns, target = c(0.04, 0.01)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_covariance(
      returns,
      target = matrix(c("a", "b", "c", "d"), nrow = 2)
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_covariance(
      returns,
      target = matrix(seq_len(6), nrow = 2, ncol = 3)
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_covariance(
      returns,
      target = matrix(0.04, nrow = 1, ncol = 1)
    ),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_covariance() rejects missing and non-finite target values", {
  missing_target <- diag(c(0.04, 0.01))
  missing_target[1, 2] <- NA_real_
  missing_target[2, 1] <- NA_real_

  infinite_target <- diag(c(0.04, 0.01))
  infinite_target[1, 2] <- Inf
  infinite_target[2, 1] <- Inf

  expect_error(
    view_covariance(returns, target = missing_target),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_covariance(returns, target = infinite_target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_covariance() requires a symmetric target matrix", {
  target <- matrix(
    c(
      0.04, 0.02,
      0.01, 0.03
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    view_covariance(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_covariance() requires a positive-semidefinite target", {
  target <- matrix(
    c(
      1, 2,
      2, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    view_covariance(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_covariance() accepts positive-semidefinite singular targets", {
  target <- matrix(
    c(
      1, 1,
      1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_s3_class(
    view_covariance(returns, target = target),
    "ffp_view_covariance_spec"
  )
})


test_that("covariance target dimnames must be complete and internally consistent", {
  target <- diag(c(0.04, 0.01))
  rownames(target) <- c("DAX", "FTSE")

  expect_error(
    view_covariance(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )

  target <- diag(c(0.04, 0.01))
  dimnames(target) <- list(
    c("DAX", "FTSE"),
    c("FTSE", "DAX")
  )

  expect_error(
    view_covariance(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )

  target <- diag(c(0.04, 0.01))
  dimnames(target) <- list(
    c("DAX", "DAX"),
    c("DAX", "DAX")
  )

  expect_error(
    view_covariance(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


# Covariance-view binding -------------------------------------------------

test_that("view_covariance() binds a complete covariance block", {
  model <- make_second_moment_test_model()
  target <- make_covariance_target()

  bound_model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_covariance")
  expect_s3_class(bound_view, "ffp_view")
  expect_identical(bound_view$method, "covariance")
  expect_identical(bound_view$target, target)
  expect_identical(
    bound_view$features$names,
    c("DAX", "FTSE", "CAC")
  )
  expect_length(bound_model$views, 1)
})


test_that("view_covariance() target dimensions must match resolved features", {
  model <- make_second_moment_test_model()

  target <- diag(c(0.04, 0.01))

  expect_error(
    ffp_view(
      model,
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("named covariance targets must match feature names and order", {
  model <- make_second_moment_test_model()

  target <- make_covariance_target()
  target <- target[c("FTSE", "DAX", "CAC"), c("FTSE", "DAX", "CAC")]

  expect_error(
    ffp_view(
      model,
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("unnamed covariance targets are matched to features by position", {
  model <- make_second_moment_test_model()

  target <- unname(make_covariance_target())

  bound_model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  expect_identical(
    bound_model$views[[1]]$target,
    target
  )
})


test_that("view_covariance() requires numeric finite features", {
  model <- make_second_moment_test_model()

  target <- diag(c(0.04, 0.01))

  non_numeric <- data.frame(
    first = letters[1:5],
    second = LETTERS[1:5]
  )

  missing_panel <- cbind(
    first = c(1, 2, NA, 4, 5),
    second = seq_len(5)
  )

  infinite_panel <- cbind(
    first = c(1, 2, Inf, 4, 5),
    second = seq_len(5)
  )

  expect_error(
    ffp_view(
      model,
      view_covariance(non_numeric, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_covariance(missing_panel, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_covariance(infinite_panel, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("view_covariance() allows zero-dispersion scenario features", {
  scenarios <- data.frame(
    constant = rep(1, 5),
    variable = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform())

  target <- matrix(
    c(
      0, 0,
      0, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  bound_model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(constant, variable),
        target = target
      )
    )

  expect_s3_class(
    bound_model$views[[1]],
    "ffp_view_covariance"
  )
})


# view_correlation(): specification ---------------------------------------

test_that("view_correlation() creates a correlation-view specification", {
  target <- make_correlation_target()

  view <- view_correlation(
    x = returns,
    target = target
  )

  expect_s3_class(view, "ffp_view_correlation_spec")
  expect_s3_class(view, "ffp_view_spec")
  expect_identical(view$method, "correlation")
  expect_identical(view$target, target)
  expect_true(inherits(view$x, "quosure"))
})


test_that("view_correlation() does not evaluate x at construction time", {
  target <- matrix(
    c(
      1, 0.5,
      0.5, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  view <- view_correlation(
    x = stop("x should only be evaluated during binding"),
    target = target
  )

  expect_s3_class(view, "ffp_view_correlation_spec")

  model <- make_second_moment_test_model()

  expect_error(
    ffp_view(model, view),
    class = "ffp_error_invalid_view_expression"
  )
})


test_that("view_correlation() requires x and target", {
  target <- diag(2)

  expect_error(
    view_correlation(target = target),
    class = "ffp_error_invalid_view"
  )

  expect_error(
    view_correlation(x = returns),
    class = "ffp_error_invalid_view"
  )
})


test_that("view_correlation() requires a numeric square matrix target", {
  expect_error(
    view_correlation(returns, target = c(1, 0.5)),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_correlation(
      returns,
      target = matrix(c("1", "0", "0", "1"), nrow = 2)
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_correlation(
      returns,
      target = matrix(seq_len(6), nrow = 2, ncol = 3)
    ),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_correlation(
      returns,
      target = matrix(1, nrow = 1, ncol = 1)
    ),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_correlation() rejects missing and non-finite target values", {
  missing_target <- diag(2)
  missing_target[1, 2] <- NA_real_
  missing_target[2, 1] <- NA_real_

  infinite_target <- diag(2)
  infinite_target[1, 2] <- Inf
  infinite_target[2, 1] <- Inf

  expect_error(
    view_correlation(returns, target = missing_target),
    class = "ffp_error_invalid_view_target"
  )

  expect_error(
    view_correlation(returns, target = infinite_target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("correlation target entries must be between -1 and 1", {
  target <- matrix(
    c(
      1, 1.1,
      1.1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_correlation() requires a symmetric target matrix", {
  target <- matrix(
    c(
      1, 0.6,
      0.5, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_correlation() requires a unit diagonal", {
  target <- matrix(
    c(
      1, 0.5,
      0.5, 0.9
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_correlation() requires a positive-semidefinite target", {
  target <- matrix(
    c(
      1.0, 0.9, 0.9,
      0.9, 1.0, -0.9,
      0.9, -0.9, 1.0
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


test_that("view_correlation() accepts boundary perfect correlations", {
  target <- matrix(
    c(
      1, -1,
      -1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  expect_s3_class(
    view_correlation(returns, target = target),
    "ffp_view_correlation_spec"
  )
})


test_that("correlation target dimnames must be complete and internally consistent", {
  target <- diag(2)
  rownames(target) <- c("DAX", "FTSE")

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )

  target <- diag(2)
  dimnames(target) <- list(
    c("DAX", "FTSE"),
    c("FTSE", "DAX")
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )

  target <- diag(2)
  dimnames(target) <- list(
    c("DAX", "DAX"),
    c("DAX", "DAX")
  )

  expect_error(
    view_correlation(returns, target = target),
    class = "ffp_error_invalid_view_target"
  )
})


# Correlation-view binding ------------------------------------------------

test_that("view_correlation() binds a complete correlation block", {
  model <- make_second_moment_test_model()
  target <- make_correlation_target()

  bound_model <- model |>
    ffp_view(
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  bound_view <- bound_model$views[[1]]

  expect_s3_class(bound_view, "ffp_view_correlation")
  expect_s3_class(bound_view, "ffp_view")
  expect_identical(bound_view$method, "correlation")
  expect_identical(bound_view$target, target)
  expect_identical(
    bound_view$features$names,
    c("DAX", "FTSE", "CAC")
  )
  expect_length(bound_model$views, 1)
})


test_that("view_correlation() target dimensions must match resolved features", {
  model <- make_second_moment_test_model()

  target <- diag(2)

  expect_error(
    ffp_view(
      model,
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("named correlation targets must match feature names and order", {
  model <- make_second_moment_test_model()

  target <- make_correlation_target()
  target <- target[c("FTSE", "DAX", "CAC"), c("FTSE", "DAX", "CAC")]

  expect_error(
    ffp_view(
      model,
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    ),
    class = "ffp_error_incompatible_view"
  )
})


test_that("unnamed correlation targets are matched to features by position", {
  model <- make_second_moment_test_model()

  target <- unname(make_correlation_target())

  bound_model <- model |>
    ffp_view(
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = target
      )
    )

  expect_identical(
    bound_model$views[[1]]$target,
    target
  )
})


test_that("view_correlation() requires numeric finite features", {
  model <- make_second_moment_test_model()

  target <- diag(2)

  non_numeric <- data.frame(
    first = letters[1:5],
    second = LETTERS[1:5]
  )

  missing_panel <- cbind(
    first = c(1, 2, NA, 4, 5),
    second = seq_len(5)
  )

  infinite_panel <- cbind(
    first = c(1, 2, Inf, 4, 5),
    second = seq_len(5)
  )

  expect_error(
    ffp_view(
      model,
      view_correlation(non_numeric, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_correlation(missing_panel, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )

  expect_error(
    ffp_view(
      model,
      view_correlation(infinite_panel, target = target)
    ),
    class = "ffp_error_invalid_view_features"
  )
})


test_that("correlation requires positive prior dispersion for every feature", {
  scenarios <- data.frame(
    constant = rep(1, 5),
    variable = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(prior_uniform())

  target <- diag(2)

  expect_error(
    ffp_view(
      model,
      view_correlation(
        x = cbind(constant, variable),
        target = target
      )
    ),
    class = "ffp_error_undefined_correlation"
  )
})


test_that("correlation reference dispersion uses the current model prior", {
  scenarios <- data.frame(
    first = c(0, 0, 1, 2),
    second = c(0, 0, 2, 4)
  )

  target <- diag(2)

  degenerate_prior_model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(c(0.5, 0.5, 0, 0))
    )

  expect_error(
    ffp_view(
      degenerate_prior_model,
      view_correlation(
        x = cbind(first, second),
        target = target
      )
    ),
    class = "ffp_error_undefined_correlation"
  )

  dispersed_prior_model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_custom(c(0.25, 0.25, 0.25, 0.25))
    ) |>
    ffp_view(
      view_correlation(
        x = cbind(first, second),
        target = target
      )
    )

  expect_s3_class(
    dispersed_prior_model$views[[1]],
    "ffp_view_correlation"
  )
})


# Constraint counting -----------------------------------------------------

test_that("matrix views create one constraint per independent matrix entry", {
  model <- make_second_moment_test_model()

  model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_correlation_target()
      )
    )

  expect_identical(
    view_constraint_count(model$views[[1]]),
    6L
  )

  expect_identical(
    view_constraint_count(model$views[[2]]),
    6L
  )
})


test_that("two-feature matrix views create three constraints", {
  model <- make_second_moment_test_model()

  covariance_target <- matrix(
    c(
      0.04, 0.01,
      0.01, 0.02
    ),
    nrow = 2,
    byrow = TRUE
  )

  correlation_target <- matrix(
    c(
      1, 0.5,
      0.5, 1
    ),
    nrow = 2,
    byrow = TRUE
  )

  model <- model |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE),
        target = covariance_target
      ),
      view_correlation(
        x = cbind(DAX, FTSE),
        target = correlation_target
      )
    )

  expect_identical(
    view_constraint_count(model$views[[1]]),
    3L
  )

  expect_identical(
    view_constraint_count(model$views[[2]]),
    3L
  )
})


# Printing ----------------------------------------------------------------

test_that("covariance and correlation specifications print their methods", {
  covariance_output <- capture.output(
    print(
      view_covariance(
        x = returns,
        target = make_covariance_target()
      )
    )
  )

  correlation_output <- capture.output(
    print(
      view_correlation(
        x = returns,
        target = make_correlation_target()
      )
    )
  )

  expect_true(
    any(grepl("Method:      covariance", covariance_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("Method:      correlation", correlation_output, fixed = TRUE))
  )
})


test_that("bound matrix views print their true constraint counts", {
  model <- make_second_moment_test_model() |>
    ffp_view(
      view_covariance(
        x = cbind(DAX, FTSE, CAC),
        target = make_covariance_target()
      ),
      view_correlation(
        x = cbind(DAX, FTSE, CAC),
        target = make_correlation_target()
      )
    )

  covariance_output <- capture.output(
    print(model$views[[1]])
  )

  correlation_output <- capture.output(
    print(model$views[[2]])
  )

  expect_true(
    any(grepl("<ffp_view_covariance>", covariance_output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Features:    3", covariance_output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Constraints: 6", covariance_output, fixed = TRUE))
  )

  expect_true(
    any(grepl("<ffp_view_correlation>", correlation_output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Features:    3", correlation_output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Constraints: 6", correlation_output, fixed = TRUE))
  )
})
