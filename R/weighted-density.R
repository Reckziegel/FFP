# Weighted kernel densities -----------------------------------------------

weighted_density_bandwidth <- function(
    values,
    adjust = 1,
    call = rlang::caller_env()
) {
  validate_density_values(
    values,
    call = call
  )

  validate_density_adjust(
    adjust,
    call = call
  )

  if (length(unique(values)) < 2L) {
    cli::cli_abort(
      c(
        "A kernel density cannot be estimated from a constant feature.",
        "i" = "Use {.code type = \"histogram\"} for constant features."
      ),
      class = "ffp_error_degenerate_density",
      call = call
    )
  }

  bandwidth <- stats::bw.nrd0(values) * adjust

  validate_density_bandwidth(
    bandwidth,
    call = call
  )

  unname(bandwidth)
}


weighted_density_grid <- function(
    values,
    bandwidth,
    n = 512L,
    call = rlang::caller_env()
) {
  validate_density_values(
    values,
    call = call
  )

  validate_density_bandwidth(
    bandwidth,
    call = call
  )

  validate_density_n(
    n,
    call = call
  )

  n <- as.integer(n)

  extension <- 4 * bandwidth

  seq(
    from = min(values) - extension,
    to = max(values) + extension,
    length.out = n
  )
}


weighted_density <- function(
    values,
    probabilities,
    grid,
    bandwidth,
    call = rlang::caller_env()
) {
  validate_density_values(
    values,
    call = call
  )

  validate_density_probabilities(
    probabilities,
    n_values = length(values),
    call = call
  )

  validate_density_grid(
    grid,
    call = call
  )

  validate_density_bandwidth(
    bandwidth,
    call = call
  )

  density <- vapply(
    grid,
    function(point) {
      kernel <- stats::dnorm(
        (point - values) / bandwidth
      )

      sum(probabilities * kernel) / bandwidth
    },
    FUN.VALUE = numeric(1)
  )

  tibble::tibble(
    x = grid,
    density = density
  )
}


weighted_density_at <- function(
    values,
    probabilities,
    x,
    bandwidth,
    call = rlang::caller_env()
) {
  validate_density_values(
    values,
    call = call
  )

  validate_density_probabilities(
    probabilities,
    n_values = length(values),
    call = call
  )

  validate_density_evaluation_points(
    x,
    call = call
  )

  validate_density_bandwidth(
    bandwidth,
    call = call
  )

  vapply(
    x,
    function(point) {
      kernel <- stats::dnorm(
        (point - values) / bandwidth
      )

      sum(probabilities * kernel) / bandwidth
    },
    FUN.VALUE = numeric(1)
  )
}


weighted_distribution_densities <- function(
    values,
    probabilities,
    adjust = 1,
    n = 512L,
    call = rlang::caller_env()
) {
  validate_density_values(
    values,
    call = call
  )

  validate_density_probability_table(
    probabilities,
    n_values = length(values),
    call = call
  )

  bandwidth <- weighted_density_bandwidth(
    values = values,
    adjust = adjust,
    call = call
  )

  grid <- weighted_density_grid(
    values = values,
    bandwidth = bandwidth,
    n = n,
    call = call
  )

  distributions <- density_distribution_names(
    probabilities
  )

  densities <- purrr::map(
    distributions,
    \(distribution) {
      weighted_density(
        values = values,
        probabilities = probabilities[[distribution]],
        grid = grid,
        bandwidth = bandwidth,
        call = call
      ) |>
        dplyr::mutate(
          distribution = distribution,
          bandwidth = bandwidth,
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
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "distribution",
          "x",
          "density",
          "bandwidth"
        )
      )
    )

  densities
}


density_distribution_names <- function(probabilities) {
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

validate_density_values <- function(
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
        "i" = "Kernel densities require finite numeric scenario values."
      ),
      class = "ffp_error_invalid_density_values",
      call = call
    )
  }

  invisible(values)
}


validate_density_probabilities <- function(
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
      class = "ffp_error_invalid_density_probabilities",
      call = call
    )
  }

  invisible(probabilities)
}


validate_density_probability_table <- function(
    probabilities,
    n_values,
    call = rlang::caller_env()
) {
  if (!is.data.frame(probabilities)) {
    cli::cli_abort(
      "{.arg probabilities} must be a data frame.",
      class = "ffp_error_invalid_density_probability_table",
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
      class = "ffp_error_invalid_density_probability_table",
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
      class = "ffp_error_invalid_density_probability_table",
      call = call
    )
  }

  distributions <- density_distribution_names(
    probabilities
  )

  purrr::walk(
    distributions,
    \(distribution) {
      validate_density_probabilities(
        probabilities = probabilities[[distribution]],
        n_values = n_values,
        call = call
      )
    }
  )

  invisible(probabilities)
}


validate_density_adjust <- function(
    adjust,
    call = rlang::caller_env()
) {
  valid <- is.numeric(adjust) &&
    is.null(dim(adjust)) &&
    length(adjust) == 1L &&
    !is.na(adjust) &&
    is.finite(adjust) &&
    adjust > 0

  if (!valid) {
    cli::cli_abort(
      "{.arg adjust} must be a single positive finite number.",
      class = "ffp_error_invalid_density_adjust",
      call = call
    )
  }

  invisible(adjust)
}


validate_density_bandwidth <- function(
    bandwidth,
    call = rlang::caller_env()
) {
  valid <- is.numeric(bandwidth) &&
    is.null(dim(bandwidth)) &&
    length(bandwidth) == 1L &&
    !is.na(bandwidth) &&
    is.finite(bandwidth) &&
    bandwidth > 0

  if (!valid) {
    cli::cli_abort(
      "The kernel density bandwidth must be positive and finite.",
      class = "ffp_error_invalid_density_bandwidth",
      call = call
    )
  }

  invisible(bandwidth)
}


validate_density_n <- function(
    n,
    call = rlang::caller_env()
) {
  valid <- is.numeric(n) &&
    is.null(dim(n)) &&
    length(n) == 1L &&
    !is.na(n) &&
    is.finite(n) &&
    n >= 2 &&
    n == floor(n)

  if (!valid) {
    cli::cli_abort(
      "{.arg n} must be a whole number greater than or equal to 2.",
      class = "ffp_error_invalid_density_n",
      call = call
    )
  }

  invisible(n)
}


validate_density_grid <- function(
    grid,
    call = rlang::caller_env()
) {
  valid <- is.numeric(grid) &&
    is.null(dim(grid)) &&
    length(grid) >= 2L &&
    all(is.finite(grid)) &&
    all(diff(grid) > 0)

  if (!valid) {
    cli::cli_abort(
      "{.arg grid} must be a strictly increasing finite numeric vector.",
      class = "ffp_error_invalid_density_grid",
      call = call
    )
  }

  invisible(grid)
}


validate_density_evaluation_points <- function(
    x,
    call = rlang::caller_env()
) {
  valid <- is.numeric(x) &&
    is.null(dim(x)) &&
    length(x) > 0L &&
    all(is.finite(x))

  if (!valid) {
    cli::cli_abort(
      "{.arg x} must be a finite numeric vector.",
      class = "ffp_error_invalid_density_points",
      call = call
    )
  }

  invisible(x)
}
