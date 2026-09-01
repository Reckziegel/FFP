# Distribution plot data --------------------------------------------------

prepare_distribution_plot_data <- function(
    object,
    feature,
    bins = NULL,
    call = rlang::caller_env()
) {
  model <- distribution_plot_model(
    object = object,
    call = call
  )

  feature <- validate_distribution_feature_name(
    feature = feature,
    call = call
  )

  values <- distribution_feature_values(
    model = model,
    feature = feature,
    call = call
  )

  probabilities <- ffp_probabilities(object)

  histogram <- weighted_distribution_histograms(
    values = values,
    probabilities = probabilities,
    bins = bins,
    call = call
  )

  distribution_names <- levels(
    histogram$distribution
  )

  distribution_labels <- c(
    prior = "Prior",
    full_confidence = "Full confidence",
    posterior = "Posterior"
  )

  histogram |>
    dplyr::mutate(
      feature = feature,
      distribution = factor(
        as.character(.data$distribution),
        levels = distribution_names,
        labels = unname(
          distribution_labels[distribution_names]
        )
      ),
      .before = 1
    )
}


# Object resolution -------------------------------------------------------

distribution_plot_model <- function(
    object,
    call = rlang::caller_env()
) {
  if (inherits(object, "ffp_fit")) {
    validate_ffp_fit(
      object,
      call = call
    )

    return(object$model)
  }

  if (inherits(object, "ffp_opinion_pool")) {
    if (!inherits(object$fit, "ffp_fit")) {
      ffp_abort(
        "The opinion pool does not contain a valid {.cls ffp_fit}.",
        class = "ffp_error_invalid_distribution_plot",
        call = call
      )
    }

    validate_ffp_fit(
      object$fit,
      call = call
    )

    return(object$fit$model)
  }

  ffp_abort(
    c(
      "{.arg object} must be an {.cls ffp_fit} or ",
      "{.cls ffp_opinion_pool} object."
    ),
    class = "ffp_error_invalid_distribution_plot",
    call = call
  )
}


# Feature resolution ------------------------------------------------------

validate_distribution_feature_name <- function(
    feature,
    call = rlang::caller_env()
) {
  valid <- is.character(feature) &&
    is.null(dim(feature)) &&
    length(feature) == 1L &&
    !is.na(feature) &&
    nzchar(feature)

  if (!valid) {
    ffp_abort(
      c(
        "{.arg feature} must be a single non-empty character string.",
        "i" = "Select one named numeric scenario feature."
      ),
      class = "ffp_error_invalid_plot_feature",
      call = call
    )
  }

  feature
}


distribution_feature_values <- function(
    model,
    feature,
    call = rlang::caller_env()
) {
  scenarios <- model$scenarios

  feature_names <- scenario_variable_names(
    scenarios
  )

  if (
    is.null(feature_names) ||
    all(is.na(feature_names) | !nzchar(feature_names))
  ) {
    ffp_abort(
      c(
        "The scenario features are unnamed.",
        "i" = paste(
          "Distribution plots require a named numeric feature so that",
          "{.arg feature} can identify it unambiguously."
        )
      ),
      class = "ffp_error_unnamed_plot_features",
      call = call
    )
  }

  if (!feature %in% feature_names) {
    available <- paste0(
      "`",
      feature_names,
      "`",
      collapse = ", "
    )

    ffp_abort(
      c(
        "{.arg feature} does not identify a scenario feature.",
        "x" = paste0(
          "Unknown feature: `",
          feature,
          "`."
        ),
        "i" = paste0(
          "Available features: ",
          available,
          "."
        )
      ),
      class = "ffp_error_unknown_plot_feature",
      call = call
    )
  }

  data <- scenario_data_mask(
    scenarios
  )

  values <- data[[feature]]

  if (!is.numeric(values)) {
    ffp_abort(
      c(
        "{.arg feature} must identify a numeric scenario feature.",
        "x" = paste0(
          "`",
          feature,
          "` has class `",
          first_class(values),
          "`."
        ),
        "i" = "Weighted empirical histograms require numeric values."
      ),
      class = "ffp_error_non_numeric_plot_feature",
      call = call
    )
  }

  validate_histogram_values(
    values = values,
    call = call
  )

  unname(values)
}
