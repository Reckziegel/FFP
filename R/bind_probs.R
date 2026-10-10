#' Bind Flexible Probability Distributions
#'
#' Stacks flexible probability distributions in long format for comparison,
#' visualization, and tabular analysis.
#'
#' All distributions must refer to scenario supports of the same size.
#'
#' @param ... Objects of class `ffp`. Inputs can be named to identify the
#'   corresponding probability distributions in the output.
#'
#' @return A tibble with one row per scenario and probability distribution,
#'   containing:
#'
#' - `scenario`: scenario position;
#' - `distribution`: distribution identifier;
#' - `probability`: scenario probability.
#'
#' @export
bind_probs <- function(...) {
    dots <- rlang::list2(...)

    if (length(dots) == 0L) {
        cli::cli_abort("{.fn bind_probs} requires at least one {.cls ffp} object.")
    }

    valid <- vapply(dots, is_ffp, logical(1))

    if (!all(valid)) {
        invalid <- which(!valid)

        cli::cli_abort(
            c(
                "All inputs to {.fn bind_probs} must be {.cls ffp} objects.",
                "x" = "Invalid input position(s): {.val {invalid}}.",
                "i" = paste0(
                    "Use {.fn ffp_probabilities} to inspect probabilities ",
                    "from fitted FFP objects."
                )
            )
        )
    }

    sizes <- vapply(dots, vctrs::vec_size, integer(1))

    if (length(unique(sizes)) != 1L) {
        cli::cli_abort(
            c(
                "Probability distributions must have the same number of scenarios.",
                "x" = "Received sizes: {.val {sizes}}."
            )
        )
    }

    labels  <- rlang::names2(dots)
    unnamed <- !nzchar(labels)
    labels[unnamed] <- paste0("distribution_", which(unnamed))

    if (anyDuplicated(labels)) {
        cli::cli_abort("Probability distribution names must be unique.")
    }

    tables <- Map(
        function(probability, label) {
            tibble::tibble(
                scenario     = seq_len(vctrs::vec_size(probability)),
                distribution = label,
                probability  = vctrs::vec_data(probability)
            )
        },
        dots,
        labels
    )

    dplyr::bind_rows(tables)
}


