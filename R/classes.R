
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


