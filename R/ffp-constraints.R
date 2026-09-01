# Constraint objects ------------------------------------------------------

empty_constraint_matrix <- function(n_scenarios) {
  matrix(
    numeric(),
    nrow = 0L,
    ncol = n_scenarios
  )
}


empty_constraint_metadata <- function() {
  data.frame(
    constraint_type = character(),
    constraint_row = integer(),
    view_index = integer(),
    view_constraint_index = integer(),
    method = character(),
    feature_1 = character(),
    feature_2 = character(),
    condition = character(),
    label = character(),
    stringsAsFactors = FALSE
  )
}


new_constraint_metadata <- function(
    type,
    method,
    feature_1,
    label,
    feature_2 = NA_character_,
    condition = NA_character_,
    view_index = NA_integer_
) {
  n_constraints <- length(label)

  data.frame(
    constraint_type = rep(type, n_constraints),
    constraint_row = seq_len(n_constraints),
    view_index = rep(view_index, n_constraints),
    view_constraint_index = seq_len(n_constraints),
    method = rep(method, n_constraints),
    feature_1 = feature_1,
    feature_2 = rep(feature_2, length.out = n_constraints),
    condition = rep(condition, length.out = n_constraints),
    label = label,
    stringsAsFactors = FALSE
  )
}


new_ffp_constraints <- function(
    n_scenarios,
    a_eq = NULL,
    b_eq = numeric(),
    a_ineq = NULL,
    b_ineq = numeric(),
    metadata = empty_constraint_metadata()
) {
  validate_constraint_scenario_count(n_scenarios)

  n_scenarios <- as.integer(n_scenarios)

  if (is.null(a_eq)) {
    a_eq <- empty_constraint_matrix(n_scenarios)
  }

  if (is.null(a_ineq)) {
    a_ineq <- empty_constraint_matrix(n_scenarios)
  }

  constraints <- structure(
    list(
      a_eq = a_eq,
      b_eq = b_eq,
      a_ineq = a_ineq,
      b_ineq = b_ineq,
      metadata = metadata,
      n_scenarios = n_scenarios
    ),
    class = "ffp_constraints"
  )

  validate_ffp_constraints(constraints)

  constraints
}


validate_ffp_constraints <- function(
    x,
    call = rlang::caller_env()
) {
  if (!inherits(x, "ffp_constraints")) {
    ffp_abort(
      "The object must inherit from {.cls ffp_constraints}.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  validate_constraint_scenario_count(
    x$n_scenarios,
    call = call
  )

  validate_constraint_matrix(
    x$a_eq,
    n_scenarios = x$n_scenarios,
    arg = "a_eq",
    call = call
  )

  validate_constraint_rhs(
    x$b_eq,
    n_constraints = nrow(x$a_eq),
    arg = "b_eq",
    call = call
  )

  validate_constraint_matrix(
    x$a_ineq,
    n_scenarios = x$n_scenarios,
    arg = "a_ineq",
    call = call
  )

  validate_constraint_rhs(
    x$b_ineq,
    n_constraints = nrow(x$a_ineq),
    arg = "b_ineq",
    call = call
  )

  validate_constraint_metadata(
    metadata = x$metadata,
    n_equalities = nrow(x$a_eq),
    n_inequalities = nrow(x$a_ineq),
    call = call
  )

  invisible(x)
}


validate_constraint_scenario_count <- function(
    n_scenarios,
    call = rlang::caller_env()
) {
  valid <- is.numeric(n_scenarios) &&
    length(n_scenarios) == 1L &&
    !is.na(n_scenarios) &&
    is.finite(n_scenarios) &&
    n_scenarios >= 1 &&
    n_scenarios == floor(n_scenarios)

  if (!valid) {
    ffp_abort(
      "`n_scenarios` must be a positive whole number.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  invisible(n_scenarios)
}


validate_constraint_matrix <- function(
    x,
    n_scenarios,
    arg,
    call = rlang::caller_env()
) {
  if (!is.matrix(x) || !is.numeric(x)) {
    ffp_abort(
      paste0("`", arg, "` must be a numeric matrix."),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (ncol(x) != n_scenarios) {
    ffp_abort(
      c(
        paste0("`", arg, "` has an invalid scenario dimension."),
        "x" = paste0(
          "Expected ",
          n_scenarios,
          " column(s), but found ",
          ncol(x),
          "."
        )
      ),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (anyNA(x) || any(!is.finite(x))) {
    ffp_abort(
      paste0("`", arg, "` must contain only finite values."),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  invisible(x)
}


validate_constraint_rhs <- function(
    x,
    n_constraints,
    arg,
    call = rlang::caller_env()
) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    ffp_abort(
      paste0("`", arg, "` must be a numeric vector."),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (length(x) != n_constraints) {
    ffp_abort(
      c(
        paste0("`", arg, "` has an invalid length."),
        "x" = paste0(
          "Expected ",
          n_constraints,
          " value(s), but found ",
          length(x),
          "."
        )
      ),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (anyNA(x) || any(!is.finite(x))) {
    ffp_abort(
      paste0("`", arg, "` must contain only finite values."),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  invisible(x)
}


validate_constraint_metadata <- function(
    metadata,
    n_equalities,
    n_inequalities,
    call = rlang::caller_env()
) {
  required <- c(
    "constraint_type",
    "constraint_row",
    "view_index",
    "view_constraint_index",
    "method",
    "feature_1",
    "feature_2",
    "condition",
    "label"
  )

  if (!inherits(metadata, "data.frame")) {
    ffp_abort(
      "`metadata` must be a data frame.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (!all(required %in% names(metadata))) {
    ffp_abort(
      "`metadata` does not contain all required constraint fields.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  expected_rows <- n_equalities + n_inequalities

  if (nrow(metadata) != expected_rows) {
    ffp_abort(
      c(
        "`metadata` must contain one row per mathematical constraint.",
        "x" = paste0(
          "Expected ",
          expected_rows,
          " row(s), but found ",
          nrow(metadata),
          "."
        )
      ),
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (nrow(metadata) == 0L) {
    return(invisible(metadata))
  }

  valid_types <- c("equality", "inequality")

  if (
    anyNA(metadata$constraint_type) ||
    !all(metadata$constraint_type %in% valid_types)
  ) {
    ffp_abort(
      "`constraint_type` must be `equality` or `inequality`.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  equality_rows <- metadata$constraint_type == "equality"
  inequality_rows <- metadata$constraint_type == "inequality"

  if (sum(equality_rows) != n_equalities) {
    ffp_abort(
      "Equality metadata does not match `a_eq`.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (sum(inequality_rows) != n_inequalities) {
    ffp_abort(
      "Inequality metadata does not match `a_ineq`.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  expected_eq_rows <- seq_len(n_equalities)
  expected_ineq_rows <- seq_len(n_inequalities)

  if (
    !identical(
      metadata$constraint_row[equality_rows],
      expected_eq_rows
    )
  ) {
    ffp_abort(
      "Equality `constraint_row` values are inconsistent with `a_eq`.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (
    !identical(
      metadata$constraint_row[inequality_rows],
      expected_ineq_rows
    )
  ) {
    ffp_abort(
      "Inequality `constraint_row` values are inconsistent with `a_ineq`.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (
    !is.integer(metadata$view_index) ||
    any(
      !is.na(metadata$view_index) &
      metadata$view_index < 1L
    )
  ) {
    ffp_abort(
      "`view_index` must contain positive integers or missing values.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (
    !is.integer(metadata$view_constraint_index) ||
    anyNA(metadata$view_constraint_index) ||
    any(metadata$view_constraint_index < 1L)
  ) {
    ffp_abort(
      "`view_constraint_index` must contain positive integers.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  if (
    anyNA(metadata$method) ||
    any(!nzchar(metadata$method)) ||
    anyNA(metadata$label) ||
    any(!nzchar(metadata$label))
  ) {
    ffp_abort(
      "Constraint metadata must contain non-empty `method` and `label` values.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  invisible(metadata)
}


reindex_constraint_metadata <- function(metadata) {
  equality_rows <- which(
    metadata$constraint_type == "equality"
  )

  inequality_rows <- which(
    metadata$constraint_type == "inequality"
  )

  if (length(equality_rows) > 0L) {
    metadata$constraint_row[equality_rows] <- seq_along(equality_rows)
  }

  if (length(inequality_rows) > 0L) {
    metadata$constraint_row[inequality_rows] <- seq_along(inequality_rows)
  }

  rownames(metadata) <- NULL

  metadata
}


set_constraint_view_index <- function(
    constraints,
    view_index,
    call = rlang::caller_env()
) {
  valid <- is.numeric(view_index) &&
    length(view_index) == 1L &&
    !is.na(view_index) &&
    is.finite(view_index) &&
    view_index >= 1 &&
    view_index == floor(view_index)

  if (!valid) {
    ffp_abort(
      "`view_index` must be a positive whole number.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  constraints$metadata$view_index[] <- as.integer(view_index)

  validate_ffp_constraints(
    constraints,
    call = call
  )

  constraints
}


bind_ffp_constraints <- function(
    constraints,
    call = rlang::caller_env()
) {
  if (length(constraints) == 0L) {
    ffp_abort(
      "`constraints` must contain at least one constraint object.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  valid <- vapply(
    constraints,
    inherits,
    logical(1),
    what = "ffp_constraints"
  )

  if (!all(valid)) {
    ffp_abort(
      "All elements must inherit from {.cls ffp_constraints}.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  invisible(
    lapply(
      constraints,
      validate_ffp_constraints,
      call = call
    )
  )

  n_scenarios <- vapply(
    constraints,
    function(x) x$n_scenarios,
    integer(1)
  )

  if (length(unique(n_scenarios)) != 1L) {
    ffp_abort(
      "Constraint blocks must share the same scenario support.",
      class = "ffp_error_invalid_constraints",
      call = call
    )
  }

  n_scenarios <- n_scenarios[[1]]

  a_eq <- empty_constraint_matrix(n_scenarios)
  a_ineq <- empty_constraint_matrix(n_scenarios)
  b_eq <- numeric()
  b_ineq <- numeric()
  metadata <- empty_constraint_metadata()

  for (x in constraints) {
    a_eq <- rbind(a_eq, x$a_eq)
    a_ineq <- rbind(a_ineq, x$a_ineq)

    b_eq <- c(b_eq, x$b_eq)
    b_ineq <- c(b_ineq, x$b_ineq)

    metadata <- rbind(
      metadata,
      x$metadata
    )
  }

  metadata <- reindex_constraint_metadata(metadata)

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    a_ineq = a_ineq,
    b_ineq = b_ineq,
    metadata = metadata
  )
}


# Compiler ----------------------------------------------------------------

#' @noRd
compile_view <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  UseMethod("compile_view")
}


compile_views <- function(
    model,
    call = rlang::caller_env()
) {
  validate_ffp_model(
    model = model,
    call = call
  )

  if (is.null(model$prior)) {
    ffp_abort(
      c(
        "An FFP model must have a prior before views can be compiled.",
        "i" = "Specify the prior with {.fn ffp_prior} before fitting the model."
      ),
      class = "ffp_error_missing_prior",
      call = call
    )
  }

  n_scenarios <- scenario_count(model$scenarios)

  if (length(model$views) == 0L) {
    return(
      new_ffp_constraints(
        n_scenarios = n_scenarios
      )
    )
  }

  valid_views <- vapply(
    model$views,
    inherits,
    logical(1),
    what = "ffp_view"
  )

  if (!all(valid_views)) {
    ffp_abort(
      "All model views must be bound {.cls ffp_view} objects.",
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  constraints <- lapply(
    seq_along(model$views),
    function(i) {
      compiled <- compile_view(
        view = model$views[[i]],
        prior = model$prior,
        call = call
      )

      set_constraint_view_index(
        constraints = compiled,
        view_index = i,
        call = call
      )
    }
  )

  bind_ffp_constraints(
    constraints = constraints,
    call = call
  )
}


# Mean --------------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_mean <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  feature_values <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    numeric(n_scenarios)
  )

  a_eq <- t(feature_values)
  storage.mode(a_eq) <- "double"
  dimnames(a_eq) <- NULL

  b_eq <- unname(
    as.numeric(view$target)
  )

  feature_labels <- view_feature_labels(view)
  target_labels <- format_constraint_value(view$target)

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "mean",
    feature_1 = feature_labels,
    label = paste0(
      "E[",
      feature_labels,
      "] = ",
      target_labels
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}


# Probability -------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_probability <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  events <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    logical(n_scenarios)
  )

  feature_labels <- view_feature_labels(view)
  target_labels <- format_constraint_value(view$target)

  given_features <- view$parameters$given

  if (is.null(given_features)) {
    a_eq <- t(
      matrix(
        as.numeric(events),
        nrow = n_scenarios,
        ncol = n_features
      )
    )

    b_eq <- unname(
      as.numeric(view$target)
    )

    labels <- paste0(
      "P[",
      feature_labels,
      "] = ",
      target_labels
    )

    metadata <- new_constraint_metadata(
      type = "equality",
      method = "probability",
      feature_1 = feature_labels,
      label = labels
    )

    return(
      new_ffp_constraints(
        n_scenarios = n_scenarios,
        a_eq = a_eq,
        b_eq = b_eq,
        metadata = metadata
      )
    )
  }

  given <- feature_column(
    given_features,
    i = 1L
  )

  conditioning_probability <- sum(
    prior[given]
  )

  if (
    !is.finite(conditioning_probability) ||
    conditioning_probability <= 0
  ) {
    ffp_abort(
      c(
        "The conditioning event has zero probability under the current prior.",
        "x" = "A conditional probability is undefined when `P(given) = 0`.",
        "i" = paste0(
          "Revise the current prior or use a conditioning event with ",
          "positive prior support."
        )
      ),
      class = c(
        "ffp_error_zero_conditioning_probability",
        "ffp_error_incompatible_view"
      ),
      call = call
    )
  }

  a_eq <- vapply(
    seq_len(n_features),
    function(i) {
      joint <- events[, i] & given

      as.numeric(joint) -
        view$target[[i]] * as.numeric(given)
    },
    numeric(n_scenarios)
  )

  a_eq <- t(a_eq)
  dimnames(a_eq) <- NULL

  b_eq <- rep(
    0,
    n_features
  )

  condition_label <- view$metadata$given_expression

  if (
    is.null(condition_label) ||
    !nzchar(condition_label)
  ) {
    condition_label <- "given"
  }

  labels <- paste0(
    "P[",
    feature_labels,
    " | ",
    condition_label,
    "] = ",
    target_labels
  )

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "probability",
    feature_1 = feature_labels,
    condition = condition_label,
    label = labels
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}


# Quantile ----------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_quantile <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_features <- feature_count(view$features)

  levels <- resolve_quantile_levels(
    view = view,
    n_features = n_features,
    call = call
  )

  compile_quantile_like_view(
    view = view,
    levels = levels,
    method = "quantile"
  )
}


# Median ------------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_median <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_features <- feature_count(view$features)

  compile_quantile_like_view(
    view = view,
    levels = rep(0.5, n_features),
    method = "median"
  )
}



# Rank --------------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_rank <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  if (n_features < 2L) {
    ffp_abort(
      "A bound rank view must contain at least two ordered features.",
      class = "ffp_error_invalid_view_order",
      call = call
    )
  }

  feature_labels <- view_feature_labels(view)

  ordered_values <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    numeric(n_scenarios)
  )

  a_ineq <- vapply(
    seq_len(n_features - 1L),
    function(i) {
      ordered_values[, i + 1L] -
        ordered_values[, i]
    },
    numeric(n_scenarios)
  )

  a_ineq <- t(a_ineq)
  storage.mode(a_ineq) <- "double"
  dimnames(a_ineq) <- NULL

  b_ineq <- rep(
    0,
    n_features - 1L
  )

  higher_labels <- feature_labels[-n_features]
  lower_labels <- feature_labels[-1L]

  metadata <- new_constraint_metadata(
    type = "inequality",
    method = "rank",
    feature_1 = higher_labels,
    feature_2 = lower_labels,
    label = paste0(
      "E[",
      higher_labels,
      "] >= E[",
      lower_labels,
      "]"
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_ineq = a_ineq,
    b_ineq = b_ineq,
    metadata = metadata
  )
}

# Variance ----------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_variance <- function(view, prior, call = rlang::caller_env()) {
  n_scenarios <- feature_observation_count(view$features)
  n_features  <- feature_count(view$features)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  feature_values <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    numeric(n_scenarios)
  )

  reference_means <- vapply(
    seq_len(n_features),
    function(i) {
      reference_mean(
        x = feature_values[, i],
        prior = prior,
        call = call
      )
    },
    numeric(1)
  )

  a_eq <- t(feature_values^2)
  storage.mode(a_eq) <- "double"
  dimnames(a_eq) <- NULL

  targets <- unname(as.numeric(view$target))

  b_eq <- reference_means^2 + targets

  feature_labels <- view_feature_labels(view)
  target_labels  <- format_constraint_value(targets)

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "variance",
    feature_1 = feature_labels,
    label = paste0(
      "Variance[",
      feature_labels,
      "] target = ",
      target_labels
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}

# Fallback methods --------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  ffp_abort(
    c(
      "Unsupported bound view.",
      "x" = paste0(
        "No compiler is available for view method `",
        view$method,
        "`."
      )
    ),
    class = "ffp_error_invalid_view",
    call = call
  )
}


#' @exportS3Method
#' @noRd
compile_view.default <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  ffp_abort(
    "The object must inherit from {.cls ffp_view}.",
    class = "ffp_error_invalid_view",
    call = call
  )
}


# Compiler helpers --------------------------------------------------------

view_feature_labels <- function(view) {
  n_features <- feature_count(view$features)
  feature_names <- view$features$names

  if (!is.null(feature_names)) {
    return(feature_names)
  }

  expression <- view$metadata$expression

  if (
    is.null(expression) ||
    !nzchar(expression)
  ) {
    expression <- "feature"
  }

  if (n_features == 1L) {
    return(expression)
  }

  paste0(
    expression,
    "[",
    seq_len(n_features),
    "]"
  )
}


format_constraint_value <- function(x) {
  format(
    x,
    digits = 8,
    trim = TRUE
  )
}


resolve_quantile_levels <- function(
    view,
    n_features,
    call = rlang::caller_env()
) {
  levels <- as.numeric(
    view$parameters$level
  )

  if (length(levels) == 1L) {
    return(
      rep(
        levels,
        n_features
      )
    )
  }

  if (length(levels) != n_features) {
    ffp_abort(
      c(
        "The bound quantile view has incompatible `level` values.",
        "x" = paste0(
          "Expected one value or ",
          n_features,
          " values, but found ",
          length(levels),
          "."
        )
      ),
      class = "ffp_error_invalid_view_level",
      call = call
    )
  }

  levels
}


compile_quantile_like_view <- function(
    view,
    levels,
    method
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  feature_labels <- view_feature_labels(view)

  constraint_rows <- vector(
    "list",
    2L * n_features
  )

  rhs <- numeric(
    2L * n_features
  )

  metadata_feature <- character(
    2L * n_features
  )

  labels <- character(
    2L * n_features
  )

  constraint_index <- 1L

  for (i in seq_len(n_features)) {
    values <- feature_column(
      view$features,
      i = i
    )

    target <- view$target[[i]]
    level <- levels[[i]]

    lower_row <- constraint_index
    upper_row <- constraint_index + 1L

    constraint_rows[[lower_row]] <- as.numeric(
      values < target
    )

    constraint_rows[[upper_row]] <- as.numeric(
      values > target
    )

    rhs[[lower_row]] <- level
    rhs[[upper_row]] <- 1 - level

    metadata_feature[[lower_row]] <- feature_labels[[i]]
    metadata_feature[[upper_row]] <- feature_labels[[i]]

    target_label <- format_constraint_value(target)
    level_label <- format_constraint_value(level)
    upper_level_label <- format_constraint_value(1 - level)

    labels[[lower_row]] <- paste0(
      "P[",
      feature_labels[[i]],
      " < ",
      target_label,
      "] <= ",
      level_label
    )

    labels[[upper_row]] <- paste0(
      "P[",
      feature_labels[[i]],
      " > ",
      target_label,
      "] <= ",
      upper_level_label
    )

    constraint_index <- constraint_index + 2L
  }

  a_ineq <- do.call(
    rbind,
    constraint_rows
  )

  storage.mode(a_ineq) <- "double"
  dimnames(a_ineq) <- NULL

  metadata <- new_constraint_metadata(
    type = "inequality",
    method = method,
    feature_1 = metadata_feature,
    label = labels
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_ineq = a_ineq,
    b_ineq = rhs,
    metadata = metadata
  )
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_constraints <- function(x, ...) {
  validate_ffp_constraints(x)

  cat("<ffp_constraints>\n")
  cat("Scenarios:    ", x$n_scenarios, "\n", sep = "")
  cat("Equalities:   ", nrow(x$a_eq), "\n", sep = "")
  cat("Inequalities: ", nrow(x$a_ineq), "\n", sep = "")

  invisible(x)
}


# Reference moments -------------------------------------------------------

validate_reference_prior <- function(
    prior,
    n_scenarios,
    call = rlang::caller_env()
) {
  valid <- is.numeric(prior) &&
    is.null(dim(prior)) &&
    length(prior) == n_scenarios &&
    !anyNA(prior) &&
    all(is.finite(prior)) &&
    all(prior >= 0)

  if (!valid) {
    ffp_abort(
      c(
        "The current prior is incompatible with the scenario support.",
        "x" = paste0(
          "Expected ",
          n_scenarios,
          " finite, non-negative probability value(s)."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  prior_sum <- sum(prior)

  if (!is.finite(prior_sum) || prior_sum <= 0) {
    ffp_abort(
      "The current prior must have positive total probability.",
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(prior)
}


reference_mean <- function(
    x,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- length(x)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  sum(prior * x) / sum(prior)
}


reference_second_moment <- function(
    x,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- length(x)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  sum(prior * x^2) / sum(prior)
}



# Reference variance ------------------------------------------------------

reference_variance <- function(
    x,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- length(x)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  mean <- reference_mean(
    x = x,
    prior = prior,
    call = call
  )

  sum(prior * (x - mean)^2) / sum(prior)
}


reference_sd <- function(
    x,
    prior,
    call = rlang::caller_env()
) {
  sqrt(
    reference_variance(
      x = x,
      prior = prior,
      call = call
    )
  )
}


# Matrix-view helpers -----------------------------------------------------

matrix_constraint_pairs <- function(n_features) {
  rows <- rep(
    seq_len(n_features),
    times = rev(seq_len(n_features))
  )

  cols <- unlist(
    lapply(
      seq_len(n_features),
      function(i) {
        seq.int(i, n_features)
      }
    ),
    use.names = FALSE
  )

  cbind(
    row = as.integer(rows),
    col = as.integer(cols)
  )
}


bound_feature_matrix <- function(view) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  values <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    numeric(n_scenarios)
  )

  storage.mode(values) <- "double"
  dimnames(values) <- NULL

  values
}


reference_feature_means <- function(
    feature_values,
    prior,
    call = rlang::caller_env()
) {
  vapply(
    seq_len(ncol(feature_values)),
    function(i) {
      reference_mean(
        x = feature_values[, i],
        prior = prior,
        call = call
      )
    },
    numeric(1)
  )
}


reference_feature_sds <- function(
    feature_values,
    prior,
    call = rlang::caller_env()
) {
  vapply(
    seq_len(ncol(feature_values)),
    function(i) {
      reference_sd(
        x = feature_values[, i],
        prior = prior,
        call = call
      )
    },
    numeric(1)
  )
}


second_moment_constraint_matrix <- function(
    feature_values,
    pairs
) {
  n_scenarios <- nrow(feature_values)

  constraints <- vapply(
    seq_len(nrow(pairs)),
    function(i) {
      feature_values[, pairs[i, "row"]] *
        feature_values[, pairs[i, "col"]]
    },
    numeric(n_scenarios)
  )

  constraints <- t(constraints)

  storage.mode(constraints) <- "double"
  dimnames(constraints) <- NULL

  constraints
}


matrix_target_entries <- function(target, pairs) {
  unname(
    target[
      cbind(
        pairs[, "row"],
        pairs[, "col"]
      )
    ]
  )
}


# Covariance --------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_covariance <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  feature_values <- bound_feature_matrix(view)

  reference_means <- reference_feature_means(
    feature_values = feature_values,
    prior = prior,
    call = call
  )

  pairs <- matrix_constraint_pairs(n_features)

  a_eq <- second_moment_constraint_matrix(
    feature_values = feature_values,
    pairs = pairs
  )

  targets <- matrix_target_entries(
    target = view$target,
    pairs = pairs
  )

  row_indices <- pairs[, "row"]
  col_indices <- pairs[, "col"]

  b_eq <- reference_means[row_indices] *
    reference_means[col_indices] +
    targets

  feature_labels <- view_feature_labels(view)

  first_labels <- feature_labels[row_indices]
  second_labels <- feature_labels[col_indices]

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "covariance",
    feature_1 = first_labels,
    feature_2 = second_labels,
    label = paste0(
      "Cov[",
      first_labels,
      ", ",
      second_labels,
      "] = ",
      format_constraint_value(targets)
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}


# Correlation -------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_correlation <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  feature_values <- bound_feature_matrix(view)

  reference_means <- reference_feature_means(
    feature_values = feature_values,
    prior = prior,
    call = call
  )

  reference_sds <- reference_feature_sds(
    feature_values = feature_values,
    prior = prior,
    call = call
  )

  undefined <- !is.finite(reference_sds) |
    reference_sds <= 0

  if (any(undefined)) {
    feature_labels <- view_feature_labels(view)

    ffp_abort(
      c(
        "Correlation is undefined under the current prior.",
        "x" = paste0(
          "Feature(s) with zero reference dispersion: ",
          paste(feature_labels[undefined], collapse = ", "),
          "."
        ),
        "i" = paste0(
          "Revise the current prior or use features with positive ",
          "prior dispersion."
        )
      ),
      class = c(
        "ffp_error_undefined_correlation",
        "ffp_error_incompatible_view"
      ),
      call = call
    )
  }

  pairs <- matrix_constraint_pairs(n_features)

  a_eq <- second_moment_constraint_matrix(
    feature_values = feature_values,
    pairs = pairs
  )

  targets <- matrix_target_entries(
    target = view$target,
    pairs = pairs
  )

  row_indices <- pairs[, "row"]
  col_indices <- pairs[, "col"]

  b_eq <- reference_means[row_indices] *
    reference_means[col_indices] +
    reference_sds[row_indices] *
    reference_sds[col_indices] *
    targets

  feature_labels <- view_feature_labels(view)

  first_labels <- feature_labels[row_indices]
  second_labels <- feature_labels[col_indices]

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "correlation",
    feature_1 = first_labels,
    feature_2 = second_labels,
    label = paste0(
      "Cor[",
      first_labels,
      ", ",
      second_labels,
      "] = ",
      format_constraint_value(targets)
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}


# Volatility --------------------------------------------------------------

#' @exportS3Method
#' @noRd
compile_view.ffp_view_volatility <- function(
    view,
    prior,
    call = rlang::caller_env()
) {
  n_scenarios <- feature_observation_count(view$features)
  n_features <- feature_count(view$features)

  validate_reference_prior(
    prior = prior,
    n_scenarios = n_scenarios,
    call = call
  )

  feature_values <- vapply(
    seq_len(n_features),
    function(i) {
      feature_column(
        view$features,
        i = i
      )
    },
    numeric(n_scenarios)
  )

  reference_means <- vapply(
    seq_len(n_features),
    function(i) {
      reference_mean(
        x = feature_values[, i],
        prior = prior,
        call = call
      )
    },
    numeric(1)
  )

  a_eq <- t(feature_values^2)
  storage.mode(a_eq) <- "double"
  dimnames(a_eq) <- NULL

  targets <- unname(
    as.numeric(view$target)
  )

  b_eq <- reference_means^2 + targets^2

  feature_labels <- view_feature_labels(view)
  target_labels <- format_constraint_value(targets)

  metadata <- new_constraint_metadata(
    type = "equality",
    method = "volatility",
    feature_1 = feature_labels,
    label = paste0(
      "Volatility[",
      feature_labels,
      "] target = ",
      target_labels
    )
  )

  new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    metadata = metadata
  )
}
