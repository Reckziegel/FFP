capture_model_output <- function(model) {
  capture.output(print(model)) |>
    paste(collapse = "\n")
}


# Construction ------------------------------------------------------------

test_that("ffp_model() creates a valid model", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("x", "y"))
  )

  model <- ffp_model(scenarios)

  expect_s3_class(model, "ffp_model")
  expect_identical(model$scenarios, scenarios)

  expect_null(model$prior)
  expect_null(model$prior_spec)

  expect_type(model$views, "list")
  expect_length(model$views, 0)

  expect_identical(model$confidence, 1)

  expect_type(model$metadata, "list")
  expect_null(model$metadata$index)
  expect_null(model$metadata$index_name)
  expect_null(model$metadata$index_source)
})


test_that("ffp_model() does not assign a prior automatically", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("x", "y"))
  )

  model <- ffp_model(scenarios)

  expect_null(model$prior)
  expect_null(model$prior_spec)
})


# Numeric vectors ---------------------------------------------------------

test_that("ffp_model() treats a numeric vector as univariate scenarios", {
  scenarios <- c(-0.03, 0.01, 0.02)

  model <- ffp_model(scenarios)

  expect_identical(model$scenarios, scenarios)
  expect_identical(scenario_count(model$scenarios), 3L)
  expect_identical(scenario_variable_count(model$scenarios), 1L)

  expect_null(model$metadata$index)
  expect_null(model$metadata$index_name)
  expect_null(model$metadata$index_source)
})


# Matrices ----------------------------------------------------------------

test_that("ffp_model() preserves a matrix", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("equity", "volatility"))
  )

  model <- ffp_model(scenarios)

  expect_true(is.matrix(model$scenarios))
  expect_identical(model$scenarios, scenarios)
})


test_that("ffp_model() does not create artificial variable names", {
  scenarios <- matrix(1:6, nrow = 3)

  model <- ffp_model(scenarios)

  expect_null(colnames(model$scenarios))
})


test_that("ffp_model() accepts completely unnamed matrix variables", {
  scenarios <- matrix(1:6, nrow = 3)
  colnames(scenarios) <- c("", "")

  model <- ffp_model(scenarios)

  expect_identical(colnames(model$scenarios), c("", ""))
})


test_that("ffp_model() uses matrix row names as scenario index", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(
      c("base", "stress_1", "stress_2"),
      c("equity", "volatility")
    )
  )

  model <- ffp_model(scenarios)

  expect_identical(
    model$metadata$index,
    c("base", "stress_1", "stress_2")
  )

  expect_null(model$metadata$index_name)
  expect_identical(model$metadata$index_source, "rownames")
})


test_that("ffp_model() parses unambiguous ISO matrix row names as Date", {
  dates <- as.Date(
    c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    )
  )

  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(
      as.character(dates),
      c("equity", "volatility")
    )
  )

  model <- ffp_model(scenarios)

  expect_s3_class(model$metadata$index, "Date")
  expect_identical(model$metadata$index, dates)

  expect_null(model$metadata$index_name)
  expect_identical(model$metadata$index_source, "rownames")
})


test_that("ffp_model() does not parse ambiguous row names as dates", {
  scenarios <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(
      c("01/02/2026", "02/02/2026"),
      c("x", "y")
    )
  )

  model <- ffp_model(scenarios)

  expect_type(model$metadata$index, "character")

  expect_identical(
    model$metadata$index,
    c("01/02/2026", "02/02/2026")
  )

  expect_identical(model$metadata$index_source, "rownames")
})


test_that("ffp_model() allows matrices without a scenario index", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("x", "y"))
  )

  model <- ffp_model(scenarios)

  expect_null(model$metadata$index)
  expect_null(model$metadata$index_name)
  expect_null(model$metadata$index_source)
})


# Data frames and tibbles -------------------------------------------------

test_that("ffp_model() preserves heterogeneous data frames", {
  scenarios <- data.frame(
    spread = factor(c("low", "medium", "high")),
    default = c(FALSE, TRUE, FALSE),
    intervention = c("cut", "raise", "cut")
  )

  model <- ffp_model(scenarios)

  expect_identical(model$scenarios, scenarios)
  expect_true(is.factor(model$scenarios$spread))
  expect_type(model$scenarios$default, "logical")
  expect_type(model$scenarios$intervention, "character")
})


test_that("ffp_model() preserves tibble inputs", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    equity = c(0.01, 0.02, -0.01),
    volatility = c(0.20, 0.19, 0.24)
  )

  model <- ffp_model(scenarios)

  expect_s3_class(model$scenarios, "tbl_df")
  expect_identical(model$scenarios, scenarios)
})


test_that("ffp_model() extracts a Date column as scenario index", {
  skip_if_not_installed("tibble")

  dates <- as.Date(
    c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    )
  )

  scenarios <- tibble::tibble(
    date = dates,
    equity = c(0.01, 0.02, -0.01),
    volatility = c(0.20, 0.19, 0.24)
  )

  model <- ffp_model(scenarios)

  expect_s3_class(model$scenarios, "tbl_df")

  expect_identical(
    names(model$scenarios),
    c("equity", "volatility")
  )

  expect_identical(model$metadata$index, dates)
  expect_identical(model$metadata$index_name, "date")
  expect_identical(model$metadata$index_source, "column")
})


test_that("ffp_model() extracts a POSIXt column as scenario index", {
  skip_if_not_installed("tibble")

  timestamps <- as.POSIXct(
    c(
      "2026-01-01 10:00:00",
      "2026-01-01 11:00:00",
      "2026-01-01 12:00:00"
    ),
    tz = "UTC"
  )

  scenarios <- tibble::tibble(
    timestamp = timestamps,
    equity = c(0.01, 0.02, -0.01)
  )

  model <- ffp_model(scenarios)

  expect_identical(names(model$scenarios), "equity")
  expect_identical(model$metadata$index, timestamps)
  expect_identical(model$metadata$index_name, "timestamp")
  expect_identical(model$metadata$index_source, "column")
})


test_that("ffp_model() detects temporal columns by type, not by name", {
  skip_if_not_installed("tibble")

  dates <- as.Date(
    c(
      "2026-01-01",
      "2026-01-02"
    )
  )

  scenarios <- tibble::tibble(
    observation = dates,
    value = c(1, 2)
  )

  model <- ffp_model(scenarios)

  expect_identical(model$metadata$index, dates)
  expect_identical(model$metadata$index_name, "observation")
  expect_identical(model$metadata$index_source, "column")
})


test_that("ffp_model() does not infer dates from character columns", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    date = c("2026-01-01", "2026-01-02"),
    value = c(1, 2)
  )

  model <- ffp_model(scenarios)

  expect_identical(model$scenarios, scenarios)

  expect_null(model$metadata$index)
  expect_null(model$metadata$index_name)
  expect_null(model$metadata$index_source)
})


test_that("ffp_model() rejects multiple temporal columns", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    observation_date = as.Date(
      c("2026-01-01", "2026-01-02")
    ),
    maturity_date = as.Date(
      c("2026-06-01", "2026-06-02")
    ),
    equity = c(0.01, 0.02)
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_ambiguous_index"
  )

  expect_error(
    ffp_model(scenarios),
    "multiple temporal columns"
  )
})


test_that("ffp_model() rejects a temporal-only data frame", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    date = as.Date(
      c(
        "2026-01-01",
        "2026-01-02",
        "2026-01-03"
      )
    )
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "at least one variable"
  )
})


# xts ---------------------------------------------------------------------

test_that("ffp_model() preserves xts scenarios and stores their index", {
  skip_if_not_installed("xts")

  index <- as.Date(
    c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    )
  )

  scenarios <- xts::xts(
    matrix(
      1:6,
      nrow = 3,
      dimnames = list(
        NULL,
        c("equity", "volatility")
      )
    ),
    order.by = index
  )

  model <- ffp_model(scenarios)

  expect_s3_class(model$scenarios, "xts")
  expect_identical(model$scenarios, scenarios)

  expect_identical(
    as.character(as.Date(model$metadata$index)),
    as.character(index)
  )

  expect_null(model$metadata$index_name)
  expect_identical(model$metadata$index_source, "xts")
})


# Scenario dimensions -----------------------------------------------------

test_that("ffp_model() rejects zero scenarios", {
  scenarios <- matrix(
    numeric(),
    nrow = 0,
    ncol = 2,
    dimnames = list(NULL, c("x", "y"))
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "at least one scenario"
  )
})


test_that("ffp_model() rejects zero variables", {
  scenarios <- matrix(
    numeric(),
    nrow = 3,
    ncol = 0
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "at least one variable"
  )
})


test_that("ffp_model() rejects arrays with more than two dimensions", {
  scenarios <- array(
    seq_len(24),
    dim = c(2, 3, 4)
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "more than two dimensions"
  )
})


# Scenario values ---------------------------------------------------------

test_that("ffp_model() rejects missing scenario values", {
  scenarios <- matrix(
    c(1, NA, 3, 4),
    nrow = 2,
    dimnames = list(NULL, c("x", "y"))
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "must not contain missing values"
  )
})


test_that("ffp_model() rejects NaN scenario values", {
  scenarios <- c(1, NaN, 3)

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "must not contain missing values"
  )
})


test_that("ffp_model() rejects positive infinity", {
  scenarios <- matrix(
    c(1, Inf, 3, 4),
    nrow = 2,
    dimnames = list(NULL, c("x", "y"))
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "must be finite"
  )
})


test_that("ffp_model() rejects negative infinity", {
  scenarios <- c(1, -Inf, 3)

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "must be finite"
  )
})


test_that("ffp_model() accepts finite heterogeneous columns", {
  scenarios <- data.frame(
    numeric = c(1, 2, 3),
    logical = c(TRUE, FALSE, TRUE),
    category = factor(c("a", "b", "c"))
  )

  expect_no_error(
    ffp_model(scenarios)
  )
})


test_that("ffp_model() rejects list-columns", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    x = c(1, 2),
    nested = list(
      c(1, 2),
      c(3, 4)
    )
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "List-columns are not supported"
  )
})


# Scenario names ----------------------------------------------------------

test_that("ffp_model() rejects duplicated variable names", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("x", "x"))
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenario_names"
  )

  expect_error(
    ffp_model(scenarios),
    "must be unique"
  )
})


test_that("ffp_model() rejects partially missing matrix names", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("x", ""))
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenario_names"
  )

  expect_error(
    ffp_model(scenarios),
    "must be complete"
  )
})


test_that("ffp_model() rejects partially missing data frame names", {
  scenarios <- data.frame(
    x = 1:3,
    y = 4:6
  )

  names(scenarios) <- c("x", "")

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenario_names"
  )
})


# Scenario index validation -----------------------------------------------

test_that("ffp_model() rejects missing Date index values", {
  skip_if_not_installed("tibble")

  scenarios <- tibble::tibble(
    date = as.Date(
      c(
        "2026-01-01",
        NA,
        "2026-01-03"
      )
    ),
    value = c(1, 2, 3)
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_index"
  )

  expect_error(
    ffp_model(scenarios),
    "index must not contain missing values"
  )
})


test_that("ffp_model() does not require scenario indexes to be ordered", {
  skip_if_not_installed("tibble")

  dates <- as.Date(
    c(
      "2026-01-03",
      "2026-01-01",
      "2026-01-02"
    )
  )

  scenarios <- tibble::tibble(
    date = dates,
    value = c(3, 1, 2)
  )

  model <- ffp_model(scenarios)

  expect_identical(model$metadata$index, dates)

  expect_identical(
    model$scenarios$value,
    c(3, 1, 2)
  )
})


# Unsupported inputs ------------------------------------------------------

test_that("ffp_model() rejects unsupported inputs", {
  scenarios <- list(
    x = c(1, 2),
    y = c(3, 4)
  )

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )

  expect_error(
    ffp_model(scenarios),
    "Unsupported scenario input"
  )
})


test_that("ffp_model() rejects character vectors", {
  scenarios <- c("low", "medium", "high")

  expect_error(
    ffp_model(scenarios),
    class = "ffp_error_invalid_scenarios"
  )
})


# Printing ----------------------------------------------------------------

test_that("ffp_model() prints a compact model summary", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("equity", "volatility"))
  )

  model <- ffp_model(scenarios)
  output <- capture_model_output(model)

  expect_match(output, "<ffp_model>")

  expect_match(
    output,
    "Scenarios\\n\\s+Count:\\s+3\\n\\s+Variables:\\s+2"
  )

  expect_match(
    output,
    "Prior\\n\\s+None"
  )

  expect_match(
    output,
    "Views\\n\\s+None"
  )

  expect_match(
    output,
    "Confidence\\n\\s+Full"
  )

  expect_false(
    grepl("Method: uniform", output, fixed = TRUE)
  )

  expect_false(
    grepl("Status", output, fixed = TRUE)
  )
})


test_that("ffp_model() does not print scenario names", {
  scenarios <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(NULL, c("equity", "volatility"))
  )

  model <- ffp_model(scenarios)
  output <- capture_model_output(model)

  expect_false(
    grepl("Names:", output, fixed = TRUE)
  )

  expect_false(
    grepl("equity, volatility", output, fixed = TRUE)
  )
})


test_that("ffp_model() does not print scenario index metadata", {
  skip_if_not_installed("tibble")

  dates <- as.Date(
    c(
      "2026-01-01",
      "2026-01-02",
      "2026-01-03"
    )
  )

  scenarios <- tibble::tibble(
    date = dates,
    equity = c(0.01, 0.02, -0.01),
    volatility = c(0.20, 0.19, 0.24)
  )

  model <- ffp_model(scenarios)
  output <- capture_model_output(model)

  expect_identical(model$metadata$index, dates)
  expect_identical(model$metadata$index_name, "date")
  expect_identical(model$metadata$index_source, "column")

  expect_false(
    grepl("Index:", output, fixed = TRUE)
  )

  expect_false(
    grepl("date", output, fixed = TRUE)
  )
})


test_that("ffp_model() prints the same scenario fields across input types", {
  matrix_model <- ffp_model(
    matrix(
      1:6,
      nrow = 3,
      dimnames = list(NULL, c("x", "y"))
    )
  )

  vector_model <- ffp_model(
    c(1, 2, 3)
  )

  matrix_output <- capture_model_output(matrix_model)
  vector_output <- capture_model_output(vector_model)

  expect_match(
    matrix_output,
    "Scenarios\\n\\s+Count:\\s+3\\n\\s+Variables:\\s+2"
  )

  expect_match(
    vector_output,
    "Scenarios\\n\\s+Count:\\s+3\\n\\s+Variables:\\s+1"
  )

  expect_false(
    grepl("Index:", matrix_output, fixed = TRUE)
  )

  expect_false(
    grepl("Names:", matrix_output, fixed = TRUE)
  )

  expect_false(
    grepl("Index:", vector_output, fixed = TRUE)
  )

  expect_false(
    grepl("Names:", vector_output, fixed = TRUE)
  )
})
