# ffp_model ---------------------------------------------------------------

#' Create an FFP model
#'
#' Defines the scenario support over which a Fully Flexible Probabilities
#' model can be built.
#'
#' Each row represents one joint scenario and each column represents one
#' variable, risk driver, or state variable.
#'
#' `ffp_model()` does not assign a prior distribution automatically. Priors
#' are specified explicitly in a subsequent step of the FFP workflow.
#'
#' @param x Scenario data. Supported inputs are numeric vectors, matrices,
#'   data frames, tibbles, and `xts` objects.
#'
#' @return An object of class `ffp_model`.
#'
#' @export
#'
#' @examples
#' x <- matrix(
#'   rnorm(30),
#'   ncol = 3,
#'   dimnames = list(NULL, c("SPX", "VIX", "rates"))
#' )
#'
#' model <- ffp_model(x)
#' model
ffp_model <- function(x) {
  prepared <- prepare_scenarios(x)

  validate_scenarios(prepared$data)

  validate_scenario_index(
    index = prepared$index,
    n_scenarios = scenario_count(prepared$data)
  )

  new_ffp_model(
    scenarios = prepared$data,
    metadata = list(
      index = prepared$index,
      index_name = prepared$index_name,
      index_source = prepared$index_source
    )
  )
}


# Constructors ------------------------------------------------------------

new_ffp_model <- function(scenarios, metadata) {
  structure(
    list(
      scenarios = scenarios,
      prior = NULL,
      prior_spec = NULL,
      views = list(),
      confidence = 1,
      metadata = metadata
    ),
    class = "ffp_model"
  )
}


new_prepared_scenarios <- function(
    data,
    index = NULL,
    index_name = NULL,
    index_source = NULL
) {
  list(
    data = data,
    index = index,
    index_name = index_name,
    index_source = index_source
  )
}


# Scenario preparation ----------------------------------------------------

#' @noRd
prepare_scenarios <- function(x, call = rlang::caller_env()) {
  UseMethod("prepare_scenarios")
}


#' @exportS3Method
#' @noRd
prepare_scenarios.numeric <- function(x, call = rlang::caller_env()) {
  if (!is.null(dim(x))) {
    return(prepare_scenarios.default(x, call = call))
  }

  new_prepared_scenarios(data = x)
}


#' @exportS3Method
#' @noRd
prepare_scenarios.matrix <- function(x, call = rlang::caller_env()) {
  index <- rownames(x)

  if (is.null(index)) {
    return(new_prepared_scenarios(data = x))
  }

  index <- parse_unambiguous_date_index(index)

  new_prepared_scenarios(
    data = x,
    index = index,
    index_name = NULL,
    index_source = "rownames"
  )
}


#' @exportS3Method
#' @noRd
prepare_scenarios.array <- function(x, call = rlang::caller_env()) {
  if (length(dim(x)) > 2L) {
    ffp_abort(
      c(
        "{.arg x} must have at most two dimensions.",
        "x" = "Arrays with more than two dimensions are not supported.",
        "i" = "Each row must represent one joint scenario."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  prepare_scenarios.default(x, call = call)
}


#' @exportS3Method
#' @noRd
prepare_scenarios.data.frame <- function(x, call = rlang::caller_env()) {
  temporal <- vapply(
    x,
    function(column) {
      inherits(column, "Date") || inherits(column, "POSIXt")
    },
    logical(1)
  )

  temporal_positions <- which(temporal)

  if (length(temporal_positions) > 1L) {
    candidates <- format_column_references(
      column_names = names(x)[temporal_positions],
      positions = temporal_positions
    )

    ffp_abort(
      c(
        "Cannot determine a unique scenario index.",
        "x" = paste0(
          "{.arg x} contains multiple temporal columns: ",
          candidates,
          "."
        ),
        "i" = paste0(
          "Keep only the {.cls Date} or {.cls POSIXt} column that ",
          "identifies the scenarios."
        )
      ),
      class = "ffp_error_ambiguous_index",
      call = call
    )
  }

  if (length(temporal_positions) == 0L) {
    return(new_prepared_scenarios(data = x))
  }

  index_position <- temporal_positions[[1]]
  index <- x[[index_position]]
  index_name <- names(x)[[index_position]]

  scenarios <- x[-index_position]

  if (is.na(index_name) || !nzchar(index_name)) {
    index_name <- NULL
  }

  new_prepared_scenarios(
    data = scenarios,
    index = index,
    index_name = index_name,
    index_source = "column"
  )
}


#' @exportS3Method
#' @noRd
prepare_scenarios.xts <- function(x, call = rlang::caller_env()) {
  new_prepared_scenarios(
    data = x,
    index = stats::time(x),
    index_name = NULL,
    index_source = "xts"
  )
}


#' @exportS3Method
#' @noRd
prepare_scenarios.default <- function(x, call = rlang::caller_env()) {
  if (is.array(x) && length(dim(x)) > 2L) {
    ffp_abort(
      c(
        "{.arg x} must have at most two dimensions.",
        "x" = "Arrays with more than two dimensions are not supported.",
        "i" = "Each row must represent one joint scenario."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  if (is.numeric(x) && is.null(dim(x))) {
    return(new_prepared_scenarios(data = x))
  }

  input_class <- class(x)

  if (length(input_class) == 0L) {
    input_class <- typeof(x)
  } else {
    input_class <- input_class[[1]]
  }

  ffp_abort(
    c(
      "Unsupported scenario input.",
      "x" = paste0(
        "{.arg x} must be a numeric vector, matrix, data frame, ",
        "tibble, or {.cls xts} object."
      ),
      "i" = paste0(
        "Received an object of class `",
        input_class,
        "`."
      )
    ),
    class = "ffp_error_invalid_scenarios",
    call = call
  )
}


# Scenario validation -----------------------------------------------------

validate_scenarios <- function(x, call = rlang::caller_env()) {
  n_scenarios <- scenario_count(x)
  n_variables <- scenario_variable_count(x)

  if (n_scenarios == 0L) {
    ffp_abort(
      c(
        "{.arg x} must contain at least one scenario.",
        "i" = "Each row represents one joint scenario."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  if (n_variables == 0L) {
    ffp_abort(
      c(
        "{.arg x} must contain at least one variable.",
        "i" = "Each column represents one variable or state."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  if (inherits(x, "data.frame")) {
    list_columns <- vapply(x, is.list, logical(1))

    if (any(list_columns)) {
      column_names <- names(x)[list_columns]
      column_names <- paste0(
        "`",
        column_names,
        "`",
        collapse = ", "
      )

      ffp_abort(
        c(
          "List-columns are not supported in scenario data.",
          "x" = paste0(
            "Found list-column(s): ",
            column_names,
            "."
          ),
          "i" = paste0(
            "Each scenario variable must be stored in an atomic ",
            "column."
          )
        ),
        class = "ffp_error_invalid_scenarios",
        call = call
      )
    }
  }

  if (anyNA(x)) {
    ffp_abort(
      c(
        "{.arg x} must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value.",
        "i" = "Scenarios are never removed or imputed automatically."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  if (has_non_finite_numeric_values(x)) {
    ffp_abort(
      c(
        "Numeric scenario values must be finite.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value.",
        "i" = "Every row must represent a complete usable scenario."
      ),
      class = "ffp_error_invalid_scenarios",
      call = call
    )
  }

  validate_scenario_names(x, call = call)

  invisible(x)
}


validate_scenario_names <- function(x, call = rlang::caller_env()) {
  variable_names <- scenario_variable_names(x)

  if (is.null(variable_names)) {
    return(invisible(x))
  }

  unnamed <- is.na(variable_names) | !nzchar(variable_names)

  if (all(unnamed)) {
    return(invisible(x))
  }

  if (any(unnamed)) {
    ffp_abort(
      c(
        "Scenario variable names must be complete.",
        "x" = "Some variables are named while others are unnamed.",
        "i" = paste0(
          "Either name every scenario variable or leave all variables ",
          "unnamed."
        )
      ),
      class = "ffp_error_invalid_scenario_names",
      call = call
    )
  }

  duplicated_names <- unique(
    variable_names[duplicated(variable_names)]
  )

  if (length(duplicated_names) > 0L) {
    duplicated_names <- paste0(
      "`",
      duplicated_names,
      "`",
      collapse = ", "
    )

    ffp_abort(
      c(
        "Scenario variable names must be unique.",
        "x" = paste0(
          "Duplicated name(s): ",
          duplicated_names,
          "."
        )
      ),
      class = "ffp_error_invalid_scenario_names",
      call = call
    )
  }

  invisible(x)
}


validate_scenario_index <- function(
    index,
    n_scenarios,
    call = rlang::caller_env()
) {
  if (is.null(index)) {
    return(invisible(index))
  }

  if (length(index) != n_scenarios) {
    ffp_abort(
      c(
        "The scenario index is incompatible with the scenario data.",
        "x" = paste0(
          "The index has ",
          length(index),
          " value(s), but the model contains ",
          n_scenarios,
          " scenario(s)."
        ),
        "i" = "The index must contain exactly one value per scenario."
      ),
      class = "ffp_error_invalid_index",
      call = call
    )
  }

  if (anyNA(index)) {
    ffp_abort(
      c(
        "The scenario index must not contain missing values.",
        "x" = "Found at least one missing index value."
      ),
      class = "ffp_error_invalid_index",
      call = call
    )
  }

  invisible(index)
}


# Scenario interface ------------------------------------------------------

scenario_count <- function(x) {
  NROW(x)
}


scenario_variable_count <- function(x) {
  if (is.numeric(x) && is.null(dim(x))) {
    return(1L)
  }

  NCOL(x)
}


scenario_variable_names <- function(x) {
  if (is.numeric(x) && is.null(dim(x))) {
    return(NULL)
  }

  if (inherits(x, "data.frame")) {
    return(names(x))
  }

  colnames(x)
}


# Validation helpers ------------------------------------------------------

has_non_finite_numeric_values <- function(x) {
  if (is.numeric(x) && is.null(dim(x))) {
    return(any(!is.finite(x)))
  }

  if (is.matrix(x)) {
    return(is.numeric(x) && any(!is.finite(x)))
  }

  if (inherits(x, "xts")) {
    values <- as.matrix(x)

    return(is.numeric(values) && any(!is.finite(values)))
  }

  if (inherits(x, "data.frame")) {
    numeric_columns <- vapply(x, is.numeric, logical(1))

    if (!any(numeric_columns)) {
      return(FALSE)
    }

    return(
      any(
        vapply(
          x[numeric_columns],
          function(column) any(!is.finite(column)),
          logical(1)
        )
      )
    )
  }

  FALSE
}


parse_unambiguous_date_index <- function(index) {
  is_iso_date <- grepl(
    "^\\d{4}-\\d{2}-\\d{2}$",
    index
  )

  if (!all(is_iso_date)) {
    return(index)
  }

  parsed <- suppressWarnings(
    as.Date(index, format = "%Y-%m-%d")
  )

  if (anyNA(parsed)) {
    return(index)
  }

  parsed
}


format_column_references <- function(column_names, positions) {
  references <- ifelse(
    is.na(column_names) | !nzchar(column_names),
    paste0("column ", positions),
    paste0("`", column_names, "`")
  )

  paste(references, collapse = ", ")
}


# Conditions --------------------------------------------------------------

ffp_abort <- function(
    message,
    class,
    ...,
    call = rlang::caller_env()
) {
  cli::cli_abort(
    message = message,
    class = c(class, "ffp_error"),
    ...,
    call = call
  )
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_model <- function(x, ...) {
  n_scenarios <- scenario_count(x$scenarios)
  n_variables <- scenario_variable_count(x$scenarios)
  confidence_label <- format_model_confidence(x$confidence)

  cat("<ffp_model>\n\n")

  cat("Scenarios\n")

  cat(
    "  Count:     ",
    format(
      n_scenarios,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    "\n",
    sep = ""
  )

  cat(
    "  Variables: ",
    format(
      n_variables,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    "\n",
    sep = ""
  )

  cat("\nPrior\n")

  if (is.null(x$prior)) {
    cat("  None\n")
  } else {
    cat("  Specified\n")
  }

  cat("\nViews\n")

  if (length(x$views) == 0L) {
    cat("  None\n")
  } else {
    cat(
      "  ",
      length(x$views),
      " view(s)\n",
      sep = ""
    )
  }

  cat("\nConfidence\n")

  cat(
    "  ",
    confidence_label,
    "\n",
    sep = ""
  )

  invisible(x)
}


format_model_confidence <- function(confidence) {
  if (is.null(confidence)) {
    return("Not specified")
  }

  if (
    is.numeric(confidence) &&
    length(confidence) == 1L &&
    isTRUE(all.equal(confidence, 1))
  ) {
    return("Full")
  }

  if (
    is.numeric(confidence) &&
    length(confidence) == 1L &&
    is.finite(confidence)
  ) {
    return(
      paste0(
        format(
          confidence * 100,
          scientific = FALSE,
          trim = TRUE
        ),
        "%"
      )
    )
  }

  "Specified"
}
