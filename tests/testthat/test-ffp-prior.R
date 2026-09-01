capture_prior_output <- function(prior) {
  capture.output(print(prior)) |>
    paste(collapse = "\n")
}


# Prior specifications ----------------------------------------------------

test_that("prior_uniform() creates a uniform prior specification", {
  prior <- prior_uniform()

  expect_s3_class(prior, "ffp_prior_uniform")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "uniform")
  expect_identical(prior$parameters, list())
})


test_that("prior_custom() creates a custom prior specification", {
  probabilities <- c(0.2, 0.3, 0.5)

  prior <- prior_custom(probabilities)

  expect_s3_class(prior, "ffp_prior_custom")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "custom")
  expect_identical(prior$parameters$probabilities, probabilities)
})


test_that("prior_exp_decay() creates an exponential-decay specification", {
  prior <- prior_exp_decay(half_life = 42)

  expect_s3_class(prior, "ffp_prior_exp_decay")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "exp_decay")
  expect_identical(prior$parameters$half_life, 42)
})


test_that("prior_rolling_window() creates a rolling-window specification", {
  prior <- prior_rolling_window(window = 252)

  expect_s3_class(prior, "ffp_prior_rolling_window")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "rolling_window")
  expect_identical(prior$parameters$window, 252)
})


test_that("prior_crisp() creates a crisp-conditioning specification", {
  prior <- prior_crisp(
    inflation > 0.03,
    growth < 0
  )

  expect_s3_class(prior, "ffp_prior_crisp")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "crisp")
  expect_length(prior$parameters$conditions, 2)

  expect_s3_class(prior$parameters$conditions[[1]], "quosure")
  expect_s3_class(prior$parameters$conditions[[2]], "quosure")
})


test_that("prior_kernel() creates a kernel-conditioning specification", {
  prior <- prior_kernel(
    inflation,
    target = 0.03,
    bandwidth = 0.005
  )

  expect_s3_class(prior, "ffp_prior_kernel")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "kernel")
  expect_s3_class(prior$parameters$variable, "quosure")

  expect_identical(
    rlang::as_label(prior$parameters$variable),
    "inflation"
  )

  expect_identical(prior$parameters$target, 0.03)
  expect_identical(prior$parameters$bandwidth, 0.005)
})


test_that("prior_product() creates a product specification", {
  prior <- prior_product(
    prior_uniform(),
    prior_exp_decay(half_life = 10)
  )

  expect_s3_class(prior, "ffp_prior_product")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "product")
  expect_length(prior$parameters$priors, 2)

  expect_s3_class(
    prior$parameters$priors[[1]],
    "ffp_prior_uniform"
  )

  expect_s3_class(
    prior$parameters$priors[[2]],
    "ffp_prior_exp_decay"
  )
})


test_that("prior_mixture() creates a mixture specification", {
  prior <- prior_mixture(
    prior_uniform(),
    prior_exp_decay(half_life = 10),
    weights = c(0.7, 0.3)
  )

  expect_s3_class(prior, "ffp_prior_mixture")
  expect_s3_class(prior, "ffp_prior_spec")

  expect_identical(prior$method, "mixture")
  expect_length(prior$parameters$priors, 2)

  expect_identical(
    prior$parameters$weights,
    c(0.7, 0.3)
  )
})


test_that("prior_custom() stores probabilities as unnamed doubles", {
  prior <- prior_custom(c(0L, 1L))

  probabilities <- prior$parameters$probabilities

  expect_type(probabilities, "double")
  expect_null(names(probabilities))
  expect_identical(probabilities, c(0, 1))
})


test_that("prior_custom() allows zero probabilities", {
  prior <- prior_custom(c(0, 0.25, 0.75))

  expect_identical(
    prior$parameters$probabilities,
    c(0, 0.25, 0.75)
  )
})


test_that("prior_custom() warns and removes probability names", {
  prior <- NULL

  expect_warning(
    prior <- prior_custom(
      c(
        base = 0.4,
        stress = 0.6
      )
    ),
    class = "ffp_warning_named_probabilities"
  )

  expect_null(names(prior$parameters$probabilities))

  expect_identical(
    prior$parameters$probabilities,
    c(0.4, 0.6)
  )
})


# Custom prior validation -------------------------------------------------

test_that("prior_custom() rejects non-numeric probabilities", {
  expect_error(
    prior_custom(c("0.4", "0.6")),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects matrices", {
  probabilities <- matrix(
    c(0.4, 0.6),
    ncol = 1
  )

  expect_error(
    prior_custom(probabilities),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects empty probability vectors", {
  expect_error(
    prior_custom(numeric()),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects missing probabilities", {
  expect_error(
    prior_custom(c(0.5, NA, 0.5)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects NaN probabilities", {
  expect_error(
    prior_custom(c(0.5, NaN, 0.5)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects infinite probabilities", {
  expect_error(
    prior_custom(c(0.5, Inf)),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_custom(c(0.5, -Inf)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects negative probabilities", {
  expect_error(
    prior_custom(c(-0.1, 1.1)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() rejects probabilities that do not sum to one", {
  expect_error(
    prior_custom(c(0.2, 0.3, 0.4)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_custom() accepts negligible floating-point drift", {
  probabilities <- c(
    0.1,
    0.2,
    0.3,
    0.4 + .Machine$double.eps
  )

  prior <- prior_custom(probabilities)

  expect_equal(
    sum(prior$parameters$probabilities),
    1
  )
})


# Exponential-decay validation --------------------------------------------

test_that("prior_exp_decay() accepts positive finite half-lives", {
  expect_no_error(
    prior_exp_decay(half_life = 1)
  )

  expect_no_error(
    prior_exp_decay(half_life = 42.5)
  )
})


test_that("prior_exp_decay() stores half-life as a double", {
  prior <- prior_exp_decay(half_life = 42L)

  expect_type(prior$parameters$half_life, "double")
  expect_identical(prior$parameters$half_life, 42)
})


test_that("prior_exp_decay() rejects invalid half-lives", {
  expect_error(
    prior_exp_decay(half_life = 0),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_exp_decay(half_life = -10),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_exp_decay(half_life = Inf),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_exp_decay(half_life = NA_real_),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_exp_decay(half_life = "42"),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_exp_decay(half_life = c(20, 40)),
    class = "ffp_error_invalid_prior"
  )
})


# Rolling-window validation -----------------------------------------------

test_that("prior_rolling_window() accepts positive whole-number windows", {
  expect_no_error(
    prior_rolling_window(window = 1)
  )

  expect_no_error(
    prior_rolling_window(window = 252)
  )

  expect_no_error(
    prior_rolling_window(window = 252L)
  )
})


test_that("prior_rolling_window() stores window as a double", {
  prior <- prior_rolling_window(window = 252L)

  expect_type(prior$parameters$window, "double")
  expect_identical(prior$parameters$window, 252)
})


test_that("prior_rolling_window() rejects invalid windows", {
  expect_error(
    prior_rolling_window(window = 0),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = -5),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = 2.5),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = Inf),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = NA_real_),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = "252"),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_rolling_window(window = c(20, 40)),
    class = "ffp_error_invalid_prior"
  )
})


# Crisp-conditioning validation -------------------------------------------

test_that("prior_crisp() requires at least one condition", {
  expect_error(
    prior_crisp(),
    class = "ffp_error_invalid_prior"
  )
})


test_that("crisp conditioning requires logical results", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_crisp(value)
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("crisp conditioning rejects incompatible result sizes", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  external <- c(TRUE, FALSE)

  expect_error(
    ffp_prior(
      model,
      prior_crisp(external)
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("crisp conditioning rejects missing logical results", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  condition <- c(TRUE, NA, FALSE, TRUE)

  expect_error(
    ffp_prior(
      model,
      prior_crisp(condition)
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("crisp conditioning rejects an empty conditioning set", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_crisp(value > 100)
    ),
    class = "ffp_error_empty_conditioning_set"
  )
})


test_that("crisp conditioning reports expression evaluation errors", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_crisp(unknown_variable > 0)
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


# Kernel-conditioning validation ------------------------------------------

test_that("prior_kernel() requires a conditioning variable", {
  expect_error(
    prior_kernel(
      target = 0,
      bandwidth = 1
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_kernel() stores target and bandwidth as doubles", {
  prior <- prior_kernel(
    value,
    target = 3L,
    bandwidth = 2L
  )

  expect_type(prior$parameters$target, "double")
  expect_type(prior$parameters$bandwidth, "double")

  expect_identical(prior$parameters$target, 3)
  expect_identical(prior$parameters$bandwidth, 2)
})


test_that("prior_kernel() rejects invalid targets", {
  expect_error(
    prior_kernel(value, target = "3", bandwidth = 1),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = c(1, 2), bandwidth = 1),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = NA_real_, bandwidth = 1),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = Inf, bandwidth = 1),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_kernel() rejects invalid bandwidths", {
  expect_error(
    prior_kernel(value, target = 0, bandwidth = "1"),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = 0, bandwidth = 0),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = 0, bandwidth = -1),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = 0, bandwidth = NA_real_),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = 0, bandwidth = Inf),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_kernel(value, target = 0, bandwidth = c(1, 2)),
    class = "ffp_error_invalid_prior"
  )
})


test_that("kernel conditioning requires a numeric variable", {
  model <- ffp_model(
    data.frame(regime = c("low", "medium", "high"))
  )

  expect_error(
    ffp_prior(
      model,
      prior_kernel(
        regime,
        target = 0,
        bandwidth = 1
      )
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("kernel conditioning rejects incompatible variable sizes", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  external <- c(1, 2)

  expect_error(
    ffp_prior(
      model,
      prior_kernel(
        external,
        target = 1,
        bandwidth = 1
      )
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("kernel conditioning does not recycle scalar variables", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_kernel(
        1,
        target = 1,
        bandwidth = 1
      )
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("kernel conditioning rejects missing evaluated values", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_kernel(
        ifelse(value == 2, NA_real_, value),
        target = 2,
        bandwidth = 1
      )
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


test_that("kernel conditioning rejects non-finite evaluated values", {
  model <- ffp_model(
    data.frame(value = 1:4)
  )

  expect_error(
    ffp_prior(
      model,
      prior_kernel(
        1 / (value - 2),
        target = 0,
        bandwidth = 1
      )
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


# Product validation ------------------------------------------------------

test_that("prior_product() requires at least two priors", {
  expect_error(
    prior_product(),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_product(prior_uniform()),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_product() requires prior specifications", {
  expect_error(
    prior_product(
      prior_uniform(),
      c(0.5, 0.5)
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_product() supports programmatic splicing", {
  priors <- list(
    prior_uniform(),
    prior_exp_decay(half_life = 10)
  )

  prior <- prior_product(
    !!!priors
  )

  expect_length(
    prior$parameters$priors,
    2
  )
})


# Mixture validation ------------------------------------------------------

test_that("prior_mixture() requires at least two priors", {
  expect_error(
    prior_mixture(
      weights = numeric()
    ),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_mixture(
      prior_uniform(),
      weights = 1
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_mixture() requires explicit weights", {
  expect_error(
    prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10)
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_mixture() requires one weight per prior", {
  expect_error(
    prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10),
      weights = 1
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_mixture() rejects invalid weights", {
  expect_error(
    prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10),
      weights = c(0.6, 0.6)
    ),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10),
      weights = c(-0.2, 1.2)
    ),
    class = "ffp_error_invalid_prior"
  )

  expect_error(
    prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10),
      weights = c(NA_real_, 1)
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("prior_mixture() allows zero weights", {
  prior <- prior_mixture(
    prior_uniform(),
    prior_exp_decay(half_life = 10),
    weights = c(0, 1)
  )

  expect_identical(
    prior$parameters$weights,
    c(0, 1)
  )
})


test_that("prior_mixture() warns and removes weight names", {
  prior <- NULL

  expect_warning(
    prior <- prior_mixture(
      prior_uniform(),
      prior_exp_decay(half_life = 10),
      weights = c(uniform = 0.7, decay = 0.3)
    ),
    class = "ffp_warning_named_mixture_weights"
  )

  expect_null(
    names(prior$parameters$weights)
  )

  expect_identical(
    prior$parameters$weights,
    c(0.7, 0.3)
  )
})


test_that("prior_mixture() supports programmatic splicing", {
  priors <- list(
    prior_uniform(),
    prior_exp_decay(half_life = 10)
  )

  prior <- prior_mixture(
    !!!priors,
    weights = c(0.4, 0.6)
  )

  expect_length(
    prior$parameters$priors,
    2
  )
})


# Prior realization -------------------------------------------------------

test_that("ffp_prior() applies a uniform prior", {
  model <- ffp_model(
    c(-0.02, 0.01, 0.03)
  ) |>
    ffp_prior(
      prior_uniform()
    )

  expect_identical(
    model$prior,
    rep(1 / 3, 3)
  )
})


test_that("ffp_prior() applies a custom prior", {
  model <- ffp_model(
    c(-0.02, 0.01, 0.03)
  ) |>
    ffp_prior(
      prior_custom(
        c(0.2, 0.3, 0.5)
      )
    )

  expect_identical(
    model$prior,
    c(0.2, 0.3, 0.5)
  )
})


test_that("ffp_prior() applies exponential decay", {
  model <- ffp_model(
    c(-0.02, 0.01, 0.03)
  ) |>
    ffp_prior(
      prior_exp_decay(half_life = 1)
    )

  expect_equal(
    model$prior,
    c(1 / 7, 2 / 7, 4 / 7)
  )
})


test_that("ffp_prior() applies a rolling-window prior", {
  model <- ffp_model(
    c(1, 2, 3, 4, 5)
  ) |>
    ffp_prior(
      prior_rolling_window(window = 3)
    )

  expect_identical(
    model$prior,
    c(0, 0, 1 / 3, 1 / 3, 1 / 3)
  )
})


test_that("ffp_prior() applies crisp conditioning", {
  model <- ffp_model(
    data.frame(value = 1:5)
  ) |>
    ffp_prior(
      prior_crisp(value >= 3)
    )

  expect_identical(
    model$prior,
    c(0, 0, 1 / 3, 1 / 3, 1 / 3)
  )
})


test_that("ffp_prior() applies kernel conditioning", {
  model <- ffp_model(
    data.frame(value = c(0, 1, 2))
  ) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 1,
        bandwidth = 1
      )
    )

  weights <- c(
    exp(-0.5),
    1,
    exp(-0.5)
  )

  expect_equal(
    model$prior,
    weights / sum(weights)
  )
})


# Exponential-decay behavior ----------------------------------------------

test_that("exponential decay gives later scenarios greater probability", {
  model <- ffp_model(
    c(1, 2, 3, 4)
  ) |>
    ffp_prior(
      prior_exp_decay(half_life = 2)
    )

  expect_true(
    all(diff(model$prior) > 0)
  )
})


test_that("exponential decay respects its half-life", {
  model <- ffp_model(
    c(1, 2, 3, 4, 5)
  ) |>
    ffp_prior(
      prior_exp_decay(half_life = 2)
    )

  expect_equal(
    model$prior[[3]] / model$prior[[5]],
    0.5
  )
})


test_that("extreme exponential decay preserves positive support", {
  model <- ffp_model(
    seq_len(1000)
  ) |>
    ffp_prior(
      prior_exp_decay(
        half_life = 0.01
      )
    )

  expect_true(
    all(model$prior > 0)
  )

  expect_equal(
    sum(model$prior),
    1
  )
})


# Rolling-window behavior -------------------------------------------------

test_that("rolling window assigns zero probability outside the window", {
  model <- ffp_model(
    c(1, 2, 3, 4, 5)
  ) |>
    ffp_prior(
      prior_rolling_window(window = 2)
    )

  expect_identical(
    model$prior,
    c(0, 0, 0, 0.5, 0.5)
  )
})


test_that("rolling window of one selects only the last scenario", {
  model <- ffp_model(
    c(1, 2, 3, 4)
  ) |>
    ffp_prior(
      prior_rolling_window(window = 1)
    )

  expect_identical(
    model$prior,
    c(0, 0, 0, 1)
  )
})


test_that("rolling window equal to support size is uniform", {
  model <- ffp_model(
    c(1, 2, 3, 4)
  ) |>
    ffp_prior(
      prior_rolling_window(window = 4)
    )

  expect_identical(
    model$prior,
    rep(0.25, 4)
  )
})


test_that("rolling window rejects a window larger than the support", {
  model <- ffp_model(
    c(1, 2, 3)
  )

  expect_error(
    ffp_prior(
      model,
      prior_rolling_window(window = 4)
    ),
    class = "ffp_error_incompatible_prior"
  )
})


# Crisp-conditioning behavior ---------------------------------------------

test_that("crisp conditioning combines multiple expressions with AND", {
  scenarios <- data.frame(
    inflation = c(0.01, 0.02, 0.03, 0.04, 0.05),
    growth = c(0.03, 0.01, -0.01, -0.02, 0.01)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_crisp(
        inflation >= 0.03,
        growth < 0
      )
    )

  expect_identical(
    model$prior,
    c(0, 0, 0.5, 0.5, 0)
  )
})


test_that("crisp conditioning supports mean and quantile", {
  scenarios <- data.frame(
    variable = 1:10
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_crisp(
        variable >= mean(variable),
        variable < quantile(variable, 0.9)
      )
    )

  expect_identical(
    model$prior,
    c(
      0, 0, 0, 0, 0,
      0.25, 0.25, 0.25, 0.25, 0
    )
  )
})


test_that("crisp conditioning supports categorical expressions", {
  scenarios <- data.frame(
    regime = c(
      "expansion",
      "recession",
      "crisis",
      "expansion"
    )
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_crisp(
        regime %in% c("recession", "crisis")
      )
    )

  expect_identical(
    model$prior,
    c(0, 0.5, 0.5, 0)
  )
})


test_that("crisp conditioning supports logical variables", {
  scenarios <- data.frame(
    stressed = c(FALSE, TRUE, FALSE, TRUE)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_crisp(stressed)
    )

  expect_identical(
    model$prior,
    c(0, 0.5, 0, 0.5)
  )
})


test_that("crisp conditioning supports curly-curly programming", {
  make_prior <- function(variable, threshold) {
    prior_crisp(
      {{ variable }} >= threshold
    )
  }

  scenarios <- data.frame(
    inflation = c(0.01, 0.02, 0.03, 0.04)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      make_prior(
        inflation,
        0.03
      )
    )

  expect_identical(
    model$prior,
    c(0, 0, 0.5, 0.5)
  )
})


test_that("crisp conditioning supports .data string indirection", {
  make_prior <- function(variable, threshold) {
    prior_crisp(
      .data[[variable]] >= threshold
    )
  }

  scenarios <- data.frame(
    inflation = c(0.01, 0.02, 0.03, 0.04)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      make_prior(
        "inflation",
        0.03
      )
    )

  expect_identical(
    model$prior,
    c(0, 0, 0.5, 0.5)
  )
})


# Kernel-conditioning behavior --------------------------------------------

test_that("kernel conditioning gives target scenario greatest probability", {
  scenarios <- data.frame(
    value = c(0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 1,
        bandwidth = 1
      )
    )

  expect_gt(
    model$prior[[2]],
    model$prior[[1]]
  )

  expect_gt(
    model$prior[[2]],
    model$prior[[3]]
  )
})


test_that("kernel conditioning is symmetric around the target", {
  scenarios <- data.frame(
    value = c(-2, -1, 0, 1, 2)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 0,
        bandwidth = 1
      )
    )

  expect_equal(
    model$prior[[1]],
    model$prior[[5]]
  )

  expect_equal(
    model$prior[[2]],
    model$prior[[4]]
  )
})


test_that("smaller bandwidth concentrates probability more sharply", {
  scenarios <- data.frame(
    value = c(-2, -1, 0, 1, 2)
  )

  narrow <- ffp_model(scenarios) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 0,
        bandwidth = 0.5
      )
    )

  wide <- ffp_model(scenarios) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 0,
        bandwidth = 2
      )
    )

  expect_gt(
    narrow$prior[[3]],
    wide$prior[[3]]
  )
})


test_that("extreme kernel conditioning preserves positive support", {
  scenarios <- data.frame(
    value = c(-10, -5, 0, 5, 10)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_kernel(
        value,
        target = 0,
        bandwidth = 0.01
      )
    )

  expect_true(
    all(model$prior > 0)
  )

  expect_identical(
    which.max(model$prior),
    3L
  )
})


test_that("kernel conditioning supports curly-curly programming", {
  make_prior <- function(variable, target, bandwidth) {
    prior_kernel(
      {{ variable }},
      target = target,
      bandwidth = bandwidth
    )
  }

  scenarios <- data.frame(
    inflation = c(0.01, 0.02, 0.03, 0.04)
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      make_prior(
        inflation,
        target = 0.03,
        bandwidth = 0.01
      )
    )

  expect_equal(
    sum(model$prior),
    1
  )
})


# Product behavior --------------------------------------------------------

test_that("product prior multiplies and normalizes component probabilities", {
  p1 <- c(0.1, 0.5, 0.4)
  p2 <- c(0.5, 0.3, 0.2)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_product(
        prior_custom(p1),
        prior_custom(p2)
      )
    )

  expected <- p1 * p2
  expected <- expected / sum(expected)

  expect_equal(
    model$prior,
    expected
  )

  expect_equal(
    model$prior,
    c(
      0.178571428571429,
      0.535714285714286,
      0.285714285714286
    )
  )
})


test_that("uniform component is neutral in a product prior", {
  probabilities <- c(0.1, 0.2, 0.7)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_product(
        prior_uniform(),
        prior_custom(probabilities)
      )
    )

  expect_equal(
    model$prior,
    probabilities
  )
})


test_that("product prior combines crisp conditioning with exponential decay", {
  scenarios <- data.frame(
    value = 1:4
  )

  model <- ffp_model(scenarios) |>
    ffp_prior(
      prior_product(
        prior_exp_decay(half_life = 1),
        prior_crisp(value >= 3)
      )
    )

  expect_equal(
    model$prior,
    c(0, 0, 1 / 3, 2 / 3)
  )
})


test_that("product prior preserves structural zeros", {
  p1 <- c(0, 0.5, 0.5)
  p2 <- c(0.4, 0.3, 0.3)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_product(
        prior_custom(p1),
        prior_custom(p2)
      )
    )

  expect_identical(
    model$prior[[1]],
    0
  )

  expect_equal(
    model$prior[2:3],
    c(0.5, 0.5)
  )
})


test_that("product prior errors when component supports do not overlap", {
  model <- ffp_model(
    c(1, 2)
  )

  prior <- prior_product(
    prior_custom(c(1, 0)),
    prior_custom(c(0, 1))
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_empty_prior_product_support"
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_incompatible_prior"
  )
})


test_that("product prior supports nested prior specifications", {
  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_product(
        prior_uniform(),
        prior_product(
          prior_custom(c(0.2, 0.3, 0.5)),
          prior_uniform()
        )
      )
    )

  expect_equal(
    model$prior,
    c(0.2, 0.3, 0.5)
  )
})


test_that("product prior validates every component against the model", {
  model <- ffp_model(
    c(1, 2, 3)
  )

  prior <- prior_product(
    prior_uniform(),
    prior_custom(c(0.4, 0.6))
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_incompatible_prior"
  )
})


# Mixture behavior --------------------------------------------------------

test_that("mixture prior computes a weighted convex combination", {
  p1 <- c(0.1, 0.5, 0.4)
  p2 <- c(0.5, 0.3, 0.2)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_mixture(
        prior_custom(p1),
        prior_custom(p2),
        weights = c(0.7, 0.3)
      )
    )

  expected <- 0.7 * p1 + 0.3 * p2

  expect_equal(
    model$prior,
    expected
  )

  expect_equal(
    model$prior,
    c(0.22, 0.44, 0.34)
  )
})


test_that("mixture prior can preserve support absent from one component", {
  p1 <- c(0, 0.5, 0.5)
  p2 <- c(0.4, 0.3, 0.3)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_mixture(
        prior_custom(p1),
        prior_custom(p2),
        weights = c(0.5, 0.5)
      )
    )

  expect_equal(
    model$prior,
    c(0.2, 0.4, 0.4)
  )

  expect_gt(
    model$prior[[1]],
    0
  )
})


test_that("zero mixture weight removes a component contribution", {
  probabilities <- c(0.1, 0.2, 0.7)

  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_mixture(
        prior_uniform(),
        prior_custom(probabilities),
        weights = c(0, 1)
      )
    )

  expect_equal(
    model$prior,
    probabilities
  )
})


test_that("mixture prior supports more than two components", {
  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_mixture(
        prior_custom(c(1, 0, 0)),
        prior_custom(c(0, 1, 0)),
        prior_custom(c(0, 0, 1)),
        weights = c(0.2, 0.3, 0.5)
      )
    )

  expect_equal(
    model$prior,
    c(0.2, 0.3, 0.5)
  )
})


test_that("mixture prior supports nested prior specifications", {
  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_mixture(
        prior_product(
          prior_uniform(),
          prior_custom(c(0.2, 0.3, 0.5))
        ),
        prior_uniform(),
        weights = c(0.75, 0.25)
      )
    )

  expected <- (
    0.75 * c(0.2, 0.3, 0.5) +
      0.25 * rep(1 / 3, 3)
  )

  expect_equal(
    model$prior,
    expected
  )
})


test_that("mixture validates every component against the model", {
  model <- ffp_model(
    c(1, 2, 3)
  )

  prior <- prior_mixture(
    prior_uniform(),
    prior_custom(c(0.4, 0.6)),
    weights = c(1, 0)
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_incompatible_prior"
  )
})


# Temporal ordering -------------------------------------------------------

test_that("time-sensitive priors accept ordered Date indexes", {
  scenarios <- data.frame(
    date = as.Date(
      c(
        "2026-01-01",
        "2026-01-02",
        "2026-01-03",
        "2026-01-04"
      )
    ),
    value = c(1, 2, 3, 4)
  )

  expect_no_error(
    ffp_model(scenarios) |>
      ffp_prior(
        prior_exp_decay(half_life = 2)
      )
  )

  expect_no_error(
    ffp_model(scenarios) |>
      ffp_prior(
        prior_rolling_window(window = 2)
      )
  )
})


test_that("time-sensitive priors reject unordered Date indexes", {
  scenarios <- data.frame(
    date = as.Date(
      c(
        "2026-01-01",
        "2026-01-03",
        "2026-01-02",
        "2026-01-04"
      )
    ),
    value = c(1, 3, 2, 4)
  )

  model <- ffp_model(scenarios)

  expect_error(
    ffp_prior(
      model,
      prior_exp_decay(half_life = 2)
    ),
    class = "ffp_error_invalid_scenario_order"
  )

  expect_error(
    ffp_prior(
      model,
      prior_rolling_window(window = 2)
    ),
    class = "ffp_error_invalid_scenario_order"
  )
})


test_that("product propagates temporal requirements of its components", {
  scenarios <- data.frame(
    date = as.Date(
      c(
        "2026-01-01",
        "2026-01-03",
        "2026-01-02"
      )
    ),
    value = c(1, 3, 2)
  )

  model <- ffp_model(scenarios)

  prior <- prior_product(
    prior_exp_decay(half_life = 2),
    prior_kernel(
      value,
      target = 2,
      bandwidth = 1
    )
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_invalid_scenario_order"
  )
})


test_that("mixture propagates temporal requirements of its components", {
  scenarios <- data.frame(
    date = as.Date(
      c(
        "2026-01-01",
        "2026-01-03",
        "2026-01-02"
      )
    ),
    value = c(1, 3, 2)
  )

  model <- ffp_model(scenarios)

  prior <- prior_mixture(
    prior_exp_decay(half_life = 2),
    prior_kernel(
      value,
      target = 2,
      bandwidth = 1
    ),
    weights = c(0.5, 0.5)
  )

  expect_error(
    ffp_prior(
      model,
      prior
    ),
    class = "ffp_error_invalid_scenario_order"
  )
})


test_that("kernel conditioning does not require temporal ordering", {
  scenarios <- data.frame(
    date = as.Date(
      c(
        "2026-01-03",
        "2026-01-01",
        "2026-01-02"
      )
    ),
    value = c(3, 1, 2)
  )

  expect_no_error(
    model <- ffp_model(scenarios) |>
      ffp_prior(
        prior_kernel(
          value,
          target = 2,
          bandwidth = 1
        )
      )
  )

  expect_equal(
    sum(model$prior),
    1
  )
})


# Model behavior ----------------------------------------------------------

test_that("ffp_prior() returns unnamed double probabilities", {
  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_uniform()
    )

  expect_type(
    model$prior,
    "double"
  )

  expect_null(
    names(model$prior)
  )
})


test_that("ffp_prior() does not mutate the original model", {
  model <- ffp_model(
    c(1, 2, 3)
  )

  updated <- model |>
    ffp_prior(
      prior_uniform()
    )

  expect_null(model$prior)
  expect_null(model$prior_spec)

  expect_identical(
    updated$prior,
    rep(1 / 3, 3)
  )
})


test_that("ffp_prior() replaces rather than composes prior calls", {
  model <- ffp_model(
    c(1, 2, 3)
  ) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_prior(
      prior_custom(
        c(0.1, 0.2, 0.7)
      )
    )

  expect_identical(
    model$prior,
    c(0.1, 0.2, 0.7)
  )

  expect_identical(
    model$prior_spec$method,
    "custom"
  )
})


test_that("ffp_prior() rejects raw probability vectors", {
  model <- ffp_model(
    c(1, 2, 3)
  )

  expect_error(
    ffp_prior(
      model,
      c(0.2, 0.3, 0.5)
    ),
    class = "ffp_error_invalid_prior"
  )
})


test_that("single-scenario priors remain valid", {
  expect_identical(
    ffp_model(1) |>
      ffp_prior(prior_uniform()) |>
      getElement("prior"),
    1
  )

  expect_identical(
    ffp_model(1) |>
      ffp_prior(prior_exp_decay(half_life = 2)) |>
      getElement("prior"),
    1
  )

  expect_identical(
    ffp_model(1) |>
      ffp_prior(prior_rolling_window(window = 1)) |>
      getElement("prior"),
    1
  )
})


# Printing ----------------------------------------------------------------

test_that("uniform prior specification has a compact print method", {
  output <- prior_uniform() |>
    capture_prior_output()

  expect_match(
    output,
    "Method:\\s+uniform"
  )
})


test_that("custom prior specification reports its size", {
  output <- prior_custom(
    c(0.2, 0.3, 0.5)
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Probabilities:\\s+3"
  )
})


test_that("exponential-decay specification reports its half-life", {
  output <- prior_exp_decay(
    half_life = 42
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Half-life:\\s+42 observations"
  )
})


test_that("rolling-window specification reports its window", {
  output <- prior_rolling_window(
    window = 252
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Window:\\s+252 observations"
  )
})


test_that("crisp specification reports condition count", {
  output <- prior_crisp(
    inflation > 0.03,
    growth < 0
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Conditions:\\s+2"
  )
})


test_that("kernel specification reports target and bandwidth", {
  output <- prior_kernel(
    inflation,
    target = 0.03,
    bandwidth = 0.005
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Variable:\\s+inflation"
  )

  expect_match(
    output,
    "Target:\\s+0.03"
  )

  expect_match(
    output,
    "Bandwidth:\\s+0.005"
  )
})


test_that("product specification prints its components", {
  output <- prior_product(
    prior_exp_decay(half_life = 42),
    prior_kernel(
      inflation,
      target = 0.03,
      bandwidth = 0.005
    )
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Method:\\s+product"
  )

  expect_match(
    output,
    "Components:\\s+2"
  )

  expect_match(
    output,
    "1\\. exponential decay"
  )

  expect_match(
    output,
    "2\\. kernel conditioning"
  )
})


test_that("mixture specification prints components and weights", {
  output <- prior_mixture(
    prior_uniform(),
    prior_exp_decay(half_life = 42),
    weights = c(0.7, 0.3)
  ) |>
    capture_prior_output()

  expect_match(
    output,
    "Method:\\s+mixture"
  )

  expect_match(
    output,
    "Components:\\s+2"
  )

  expect_match(
    output,
    "uniform \\[weight: 0.7\\]"
  )

  expect_match(
    output,
    "exponential decay \\[weight: 0.3\\]"
  )
})
