
# ffp_tbl ----------------------------------------------------------------

# constructor
#' Internal ffp class
#'
#' @param x The data to be converted to ffp.
#' @param ... Additional attributes to be passed to ffp.
#'
#' @rdname ffp
new_ffp <- function(x, ...) {
  validate_ffp(ffp(x, ...))
}

# validator
#' @keywords internal
#' @rdname ffp
validate_ffp <- function(x) {
  if (inherits(x, "grouped_df")) {
    stop("Don't know how to deal with grouped tibbles.", call. = FALSE)
  }
  tibble::validate_tibble(x)
  x
}

# helper
#' @keywords internal
#' @rdname ffp
ffp <- function(x, ...) {
  tibble::new_tibble(x = x,
                     ffp_attr = ...,
                     nrow = vctrs::vec_size(x),
                     class = "ffp")
}

#' @rdname ffp
#' @keywords internal
is_ffp <- function(x) inherits(x, "ffp")

#' @rdname ffp
#' @keywords internal
as_ffp <- function(x) new_ffp(x)

# Print ffp ----
#' @export
print.ffp <- function(x, ...) {
  cat(
    crayon::yellow("#"), crayon::bold(crayon::blue("Fully-Flexible Probability")), "\n"
    )
  class(x) <- class(x)[!(class(x) %in% "ffp")]
  print(x, ...)
}

# flex_mat ----------------------------------------------------------------

# constructor
#' Internal class ffp_mat
#'
#' @rdname ffp_mat
#' @keywords internal
new_ffp_mat <- function(.x) {
  validate_ffp_mat(ffp_mat(.x))
}

# validator
#' @rdname ffp_mat
#' @keywords internal
validate_ffp_mat <- function(.x) {
  stopifnot(is.double(.x))
  .x
}

# helper
#' @rdname ffp_mat
#' @keywords internal
ffp_mat <- function(.x, ...) {
  x <- tibble::as_tibble(.x)
  x <- dplyr::select(x, where(is.numeric))
  out <- as.matrix(x)
  out <- structure(out, class = c("matrix", "array", "ffp_mat"))
  out
}

#' @rdname ffp_mat
#' @keywords internal
is_ffp_mat <- function(.x) inherits(.x, "ffp_mat")

#' @rdname ffp_mat
#' @keywords internal
as_ffp_mat <- function(.x) ffp_mat(.x)


