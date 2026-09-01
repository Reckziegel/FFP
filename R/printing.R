# Helpers -----------------------------------------------------------------

format_view_type <- function(x) {
  type <- attr(x, "type", exact = TRUE)

  if (is.null(type) || !length(type) || is.na(type)) {
    return("Unknown")
  }

  type |>
    stringr::str_remove("^view_on_") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
}


view_component <- function(x, name) {
  if (!name %in% names(x)) {
    return(NULL)
  }

  x[[name]]
}


n_view_constraints <- function(x, name) {
  value <- view_component(x, name)

  if (is.null(value)) {
    return(0L)
  }

  NROW(value)
}


n_view_scenarios <- function(x) {
  Aeq <- view_component(x, "Aeq")
  A   <- view_component(x, "A")

  matrices <- Filter(
    Negate(is.null),
    list(Aeq, A)
  )

  if (!length(matrices)) {
    return(NA_integer_)
  }

  max(
    vapply(
      matrices,
      function(z) as.integer(NCOL(z)),
      integer(1)
    )
  )
}


format_constraint <- function(n, type) {
  if (n == 0L) {
    return(NULL)
  }

  paste(
    n,
    type,
    if (n == 1L) "constraint" else "constraints"
  )
}


format_view_dim <- function(x) {
  paste0(
    NROW(x),
    " \u00d7 ",
    NCOL(x)
  )
}


# Print methods -----------------------------------------------------------

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.ffp_views <- function(x, ...) {
  cli::cat_line(
    cli::col_cyan("# ffp view"),
    " ",
    cli::style_dim(
      paste0("<", format_view_type(x), ">")
    )
  )
}


#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.ffp_views <- function(x, ...) {
  # Number of constraints
  n_eq   <- n_view_constraints(x, "Aeq")
  n_ineq <- n_view_constraints(x, "A")

  constraints <- c(
    format_constraint(n_eq, "equality"),
    format_constraint(n_ineq, "inequality")
  )

  # Number of scenarios
  n_scenarios <- n_view_scenarios(x)

  # Print summary
  if (length(constraints)) {
    cli::cat_line(
      "  ",
      cli::style_dim(
        paste(constraints, collapse = " \u00b7 ")
      )
    )
  }

  if (!is.na(n_scenarios)) {
    cli::cat_line(
      "  ",
      cli::style_dim("Scenarios: "),
      n_scenarios
    )
  }

  # Remove NULL components, if any
  keep <- vapply(
    seq_along(x),
    function(i) !is.null(x[[i]]),
    logical(1)
  )

  values <- x[keep]

  if (!length(values)) {
    return(invisible(NULL))
  }

  cli::cat_line()

  # Component names
  nms <- names(values)

  if (is.null(nms)) {
    nms <- paste0("[[", seq_along(values), "]]")
  }

  nms <- format(nms, justify = "left")

  # Print dimensions
  for (i in seq_along(values)) {
    cli::cat_line(
      "  ",
      cli::style_bold(nms[[i]]),
      "  ",
      cli::style_dim(format_view_dim(values[[i]]))
    )
  }

  invisible(NULL)

}
