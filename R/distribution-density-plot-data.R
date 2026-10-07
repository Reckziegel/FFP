# Density plot data -------------------------------------------------------

prepare_distribution_density_plot_data <- function(
    object,
    feature,
    distributions = NULL,
    statistics = c("mean", "median"),
    quantiles = 0.05,
    adjust = 1,
    n = 512L,
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

  distributions <- resolve_distribution_plot_distributions(
    probabilities = probabilities,
    distributions = distributions,
    call = call
  )

  validate_distribution_plot_statistics(
    statistics = statistics,
    call = call
  )

  validate_distribution_plot_quantiles(
    quantiles = quantiles,
    call = call
  )

  density_data <- weighted_distribution_densities(
    values = values,
    probabilities = probabilities,
    adjust = adjust,
    n = n,
    call = call
  )

  bandwidth <- density_data$bandwidth[[1]]

  density_data <- format_distribution_density_data(
    data = density_data,
    feature = feature,
    distributions = distributions
  )

  statistics_data <- prepare_distribution_statistics_data(
    values = values,
    probabilities = probabilities,
    feature = feature,
    distributions = distributions,
    statistics = statistics,
    quantiles = quantiles,
    bandwidth = bandwidth,
    call = call
  )

  list(
    density = density_data,
    statistics = statistics_data,
    bandwidth = bandwidth
  )
}


# Distribution resolution ------------------------------------------------

resolve_distribution_plot_distributions <- function(
    probabilities,
    distributions = NULL,
    call = rlang::caller_env()
) {
  available <- density_distribution_names(
    probabilities
  )

  if (is.null(distributions)) {
    return(available)
  }

  valid <- is.character(distributions) &&
    is.null(dim(distributions)) &&
    length(distributions) > 0L &&
    !anyNA(distributions) &&
    all(nzchar(distributions)) &&
    !anyDuplicated(distributions)

  if (!valid) {
    ffp_abort(
      c(
        "{.arg distributions} must contain unique distribution names.",
        "i" = paste(
          "Use one or more of:",
          paste(
            available,
            collapse = ", "
          )
        )
      ),
      class = "ffp_error_invalid_plot_distributions",
      call = call
    )
  }

  unknown <- setdiff(
    distributions,
    available
  )

  if (length(unknown) > 0L) {
    ffp_abort(
      c(
        "{.arg distributions} contains unavailable distributions.",
        "x" = paste(
          "Unavailable:",
          paste(
            unknown,
            collapse = ", "
          )
        ),
        "i" = paste(
          "Available:",
          paste(
            available,
            collapse = ", "
          )
        )
      ),
      class = "ffp_error_invalid_plot_distributions",
      call = call
    )
  }

  distributions
}


distribution_plot_labels <- function() {
  c(
    prior = "Prior",
    full_confidence = "Full confidence",
    posterior = "Posterior"
  )
}


format_distribution_density_data <- function(
    data,
    feature,
    distributions
) {
  labels <- distribution_plot_labels()

  data |>
    dplyr::filter(
      as.character(.data$distribution) %in% distributions
    ) |>
    dplyr::mutate(
      feature = feature,
      distribution = factor(
        as.character(.data$distribution),
        levels = distributions,
        labels = unname(
          labels[distributions]
        )
      ),
      .before = 1
    ) |>
    dplyr::arrange(
      .data$distribution,
      .data$x
    )
}


# Distribution statistics ------------------------------------------------

prepare_distribution_statistics_data <- function(
    values,
    probabilities,
    feature,
    distributions,
    statistics,
    quantiles,
    bandwidth,
    call = rlang::caller_env()
) {
  labels <- distribution_plot_labels()

  statistic_labels <- distribution_plot_statistic_labels(
    statistics = statistics,
    quantiles = quantiles
  )

  if (length(statistic_labels) == 0L) {
    return(
      empty_distribution_statistics_data(
        distributions = distributions,
        distribution_labels = labels,
        statistic_labels = statistic_labels
      )
    )
  }

  data <- purrr::map(
    distributions,
    \(distribution) {
      distribution_statistics <- compute_distribution_statistics(
        values = values,
        probabilities = probabilities[[distribution]],
        statistics = statistics,
        quantiles = quantiles,
        bandwidth = bandwidth,
        call = call
      )

      distribution_statistics |>
        dplyr::mutate(
          distribution = distribution,
          .before = 1
        )
    }
  ) |>
    dplyr::bind_rows()

  data |>
    dplyr::mutate(
      feature = feature,
      distribution = factor(
        .data$distribution,
        levels = distributions,
        labels = unname(
          labels[distributions]
        )
      ),
      statistic = factor(
        .data$statistic,
        levels = statistic_labels
      ),
      .before = 1
    )
}


compute_distribution_statistics <- function(
    values,
    probabilities,
    statistics,
    quantiles,
    bandwidth,
    call = rlang::caller_env()
) {
  statistic_data <- purrr::map(
    statistics,
    \(statistic) {
      value <- switch(
        statistic,
        mean = sum(
          values * probabilities
        ),
        median = unname(
          ffp_stats_lower_tail(
            x = values,
            p = probabilities,
            prob = 0.5
          )[["quantile"]]
        )
      )

      tibble::tibble(
        statistic = distribution_plot_statistic_label(
          statistic
        ),
        probability = NA_real_,
        value = value
      )
    }
  ) |>
    dplyr::bind_rows()

  quantile_data <- purrr::map(
    quantiles,
    \(probability) {
      value <- unname(
        ffp_stats_lower_tail(
          x = values,
          p = probabilities,
          prob = probability
        )[["quantile"]]
      )

      tibble::tibble(
        statistic = distribution_plot_quantile_label(
          probability
        ),
        probability = probability,
        value = value
      )
    }
  ) |>
    dplyr::bind_rows()

  data <- dplyr::bind_rows(
    statistic_data,
    quantile_data
  )

  if (nrow(data) == 0L) {
    return(
      tibble::tibble(
        statistic = character(),
        probability = numeric(),
        value = numeric(),
        density = numeric()
      )
    )
  }

  data |>
    dplyr::mutate(
      density = weighted_density_at(
        values = values,
        probabilities = probabilities,
        x = .data$value,
        bandwidth = bandwidth,
        call = call
      )
    )
}


distribution_plot_statistic_labels <- function(
    statistics,
    quantiles
) {
  statistic_labels <- vapply(
    statistics,
    distribution_plot_statistic_label,
    FUN.VALUE = character(1)
  )

  quantile_labels <- vapply(
    quantiles,
    distribution_plot_quantile_label,
    FUN.VALUE = character(1)
  )

  c(
    statistic_labels,
    quantile_labels
  )
}


distribution_plot_statistic_label <- function(statistic) {
  switch(
    statistic,
    mean = "Mean",
    median = "Median"
  )
}


distribution_plot_quantile_label <- function(probability) {
  percentage <- format(
    100 * probability,
    trim = TRUE,
    scientific = FALSE,
    digits = 6
  )

  paste0(
    percentage,
    "% quantile"
  )
}


empty_distribution_statistics_data <- function(
    distributions,
    distribution_labels,
    statistic_labels
) {
  tibble::tibble(
    feature = character(),
    distribution = factor(
      character(),
      levels = unname(
        distribution_labels[distributions]
      )
    ),
    statistic = factor(
      character(),
      levels = statistic_labels
    ),
    probability = numeric(),
    value = numeric(),
    density = numeric()
  )
}


# Validation --------------------------------------------------------------

validate_distribution_plot_statistics <- function(
    statistics,
    call = rlang::caller_env()
) {
  if (is.null(statistics)) {
    return(invisible(statistics))
  }

  valid <- is.character(statistics) &&
    is.null(dim(statistics)) &&
    !anyNA(statistics) &&
    all(nzchar(statistics)) &&
    !anyDuplicated(statistics)

  if (!valid) {
    ffp_abort(
      c(
        "{.arg statistics} must contain unique statistic names.",
        "i" = "Supported statistics are {.val mean} and {.val median}."
      ),
      class = "ffp_error_invalid_plot_statistics",
      call = call
    )
  }

  supported <- c(
    "mean",
    "median"
  )

  unknown <- setdiff(
    statistics,
    supported
  )

  if (length(unknown) > 0L) {
    ffp_abort(
      c(
        "{.arg statistics} contains unsupported statistics.",
        "x" = paste(
          "Unsupported:",
          paste(
            unknown,
            collapse = ", "
          )
        ),
        "i" = "Supported statistics are {.val mean} and {.val median}."
      ),
      class = "ffp_error_invalid_plot_statistics",
      call = call
    )
  }

  invisible(statistics)
}


validate_distribution_plot_quantiles <- function(
    quantiles,
    call = rlang::caller_env()
) {
  if (is.null(quantiles)) {
    return(invisible(quantiles))
  }

  valid <- is.numeric(quantiles) &&
    is.null(dim(quantiles)) &&
    !anyNA(quantiles) &&
    all(is.finite(quantiles)) &&
    all(quantiles > 0) &&
    all(quantiles < 1) &&
    !anyDuplicated(quantiles)

  if (!valid) {
    ffp_abort(
      c(
        "{.arg quantiles} must contain unique probabilities between 0 and 1.",
        "i" = "For example, use {.code quantiles = c(0.01, 0.05)}."
      ),
      class = "ffp_error_invalid_plot_quantiles",
      call = call
    )
  }

  invisible(quantiles)
}
