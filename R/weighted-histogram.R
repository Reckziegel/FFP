# Weighted empirical histograms -------------------------------------------

weighted_histogram_breaks <- function(
    values,
    bins = NULL,
    call = rlang::caller_env()
) {
  validate_histogram_values(
    values,
    call = call
  )

  validate_histogram_bins(
    bins,
    call = call
  )

  breaks <- if (is.null(bins)) {
    graphics::hist(
      values,
      breaks = "FD",
      plot = FALSE
    )$breaks
  } else {
    graphics::hist(
      values,
      breaks = bins,
      plot = FALSE
    )$breaks
  }

  as.numeric(breaks)
}


weighted_histogram <- function(
    values,
    probabilities,
    breaks,
    call = rlang::caller_env()
) {
  validate_histogram_values(
    values,
    call = call
  )

  validate_histogram_probabilities(
    probabilities,
    n_values = length(values),
    call = call
  )

  validate_histogram_breaks(
    breaks,
    values = values,
    call = call
  )

  n_bins <- length(breaks) - 1L

  bin <- cut(
    values,
    breaks = breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )

  bin_mass <- tibble::tibble(
    bin = bin,
    probability = probabilities
  ) |>
    dplyr::group_by(
      .data$bin
    ) |>
    dplyr::summarise(
      mass = sum(.data$probability),
      .groups = "drop"
    )

  bin_data <- tibble::tibble(
    bin = seq_len(n_bins),
    lower = breaks[-length(breaks)],
    upper = breaks[-1L]
  ) |>
    dplyr::left_join(
      bin_mass,
      by = "bin"
    ) |>
    dplyr::mutate(
      mass = dplyr::coalesce(
        .data$mass,
        0
      ),
      width = .data$upper - .data$lower,
      midpoint = (
        .data$lower +
          .data$upper
      ) / 2,
      density = .data$mass / .data$width
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "bin",
          "lower",
          "upper",
          "midpoint",
          "width",
          "mass",
          "density"
        )
      )
    )

  bin_data
}


weighted_distribution_histograms <- function(
    values,
    probabilities,
    bins = NULL,
    call = rlang::caller_env()
) {
  validate_histogram_values(
    values,
    call = call
  )

  validate_histogram_probability_table(
    probabilities,
    n_values = length(values),
    call = call
  )

  breaks <- weighted_histogram_breaks(
    values = values,
    bins = bins,
    call = call
  )

  distributions <- histogram_distribution_names(
    probabilities
  )

  histograms <- purrr::map(
    distributions,
    \(distribution) {
      weighted_histogram(
        values = values,
        probabilities = probabilities[[distribution]],
        breaks = breaks,
        call = call
      ) |>
        dplyr::mutate(
          distribution = distribution,
          .before = 1
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      distribution = factor(
        .data$distribution,
        levels = distributions
      )
    )

  histograms
}


histogram_distribution_names <- function(probabilities) {
  available <- c(
    "prior",
    "full_confidence",
    "posterior"
  )

  available[
    available %in% names(probabilities)
  ]
}


# Validation --------------------------------------------------------------

validate_histogram_values <- function(
    values,
    call = rlang::caller_env()
) {
  valid <- is.numeric(values) &&
    is.null(dim(values)) &&
    length(values) > 0L &&
    all(is.finite(values))

  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg values} must be a finite numeric vector.",
        "i" = paste(
          "Weighted histograms require finite numeric scenario",
          "values."
        )
      ),
      class = "ffp_error_invalid_histogram_values",
      call = call
    )
  }

  invisible(values)
}


validate_histogram_probabilities <- function(
    probabilities,
    n_values,
    call = rlang::caller_env()
) {
  valid <- is.numeric(probabilities) &&
    is.null(dim(probabilities)) &&
    length(probabilities) == n_values &&
    all(is.finite(probabilities)) &&
    all(probabilities >= 0)

  if (!valid) {
    cli::cli_abort(
      c(
        paste(
          "{.arg probabilities} must be a finite non-negative",
          "numeric vector."
        ),
        "i" = "It must have the same length as {.arg values}."
      ),
      class = "ffp_error_invalid_histogram_probabilities",
      call = call
    )
  }

  invisible(probabilities)
}


validate_histogram_probability_table <- function(
    probabilities,
    n_values,
    call = rlang::caller_env()
) {
  if (!is.data.frame(probabilities)) {
    cli::cli_abort(
      "{.arg probabilities} must be a data frame.",
      class = "ffp_error_invalid_histogram_probability_table",
      call = call
    )
  }

  if (nrow(probabilities) != n_values) {
    cli::cli_abort(
      c(
        "{.arg probabilities} must have one row per scenario.",
        "i" = paste0(
          "Expected ",
          n_values,
          " rows, but received ",
          nrow(probabilities),
          "."
        )
      ),
      class = "ffp_error_invalid_histogram_probability_table",
      call = call
    )
  }

  required <- c(
    "prior",
    "full_confidence"
  )

  missing <- setdiff(
    required,
    names(probabilities)
  )

  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.arg probabilities} is missing required distributions.",
        "x" = paste(
          "Missing:",
          paste(
            missing,
            collapse = ", "
          )
        )
      ),
      class = "ffp_error_invalid_histogram_probability_table",
      call = call
    )
  }

  distributions <- histogram_distribution_names(
    probabilities
  )

  purrr::walk(
    distributions,
    \(distribution) {
      validate_histogram_probabilities(
        probabilities = probabilities[[distribution]],
        n_values = n_values,
        call = call
      )
    }
  )

  invisible(probabilities)
}


validate_histogram_bins <- function(
    bins,
    call = rlang::caller_env()
) {
  if (is.null(bins)) {
    return(invisible(bins))
  }

  valid <- is.numeric(bins) &&
    is.null(dim(bins)) &&
    length(bins) == 1L &&
    is.finite(bins) &&
    bins >= 1 &&
    bins == floor(bins)

  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg bins} must be a positive integer or {.code NULL}.",
        "i" = "{.code NULL} uses the Freedman-Diaconis rule."
      ),
      class = "ffp_error_invalid_histogram_bins",
      call = call
    )
  }

  invisible(bins)
}


validate_histogram_breaks <- function(
    breaks,
    values,
    call = rlang::caller_env()
) {
  valid <- is.numeric(breaks) &&
    is.null(dim(breaks)) &&
    length(breaks) >= 2L &&
    all(is.finite(breaks)) &&
    all(diff(breaks) > 0)

  if (!valid) {
    cli::cli_abort(
      paste(
        "{.arg breaks} must be a strictly increasing finite",
        "numeric vector."
      ),
      class = "ffp_error_invalid_histogram_breaks",
      call = call
    )
  }

  covers_values <- min(breaks) <= min(values) &&
    max(breaks) >= max(values)

  if (!covers_values) {
    cli::cli_abort(
      c(
        "{.arg breaks} must cover all scenario values.",
        "i" = paste(
          "The first break must be at or below the minimum value",
          "and the last break at or above the maximum value."
        )
      ),
      class = "ffp_error_invalid_histogram_breaks",
      call = call
    )
  }

  invisible(breaks)
}
