
# ffp_tbl ----------------------------------------------------------------

# constructor
#' Internal ffp_tbl class
#'
#' @rdname ffp_tbl
#' @keywords internal
new_ffp_tbl <- function(x, ...) {
  stopifnot(tibble::is_tibble(x))
  validate_ffp_tbl(ffp_tbl(x, ...))
}

# validator
#' @rdname ffp_tbl
#' @keywords internal
validate_ffp_tbl <- function(x) {
  stopifnot(tibble::is_tibble(x))
  if (purrr::is_empty(x)) {
    stop("The argument x is empty.", call. = FALSE)
  }
  if (!any_is_double(x)) {
    stop("There is no numeric columns in x.", call. = FALSE)
  }
  if (inherits(x, "grouped_df")) {
    stop("Don't know how to deal with grouped tibbles.", call. = FALSE)
  }
  x
}

# helper
#' @rdname ffp_tbl
#' @keywords internal
ffp_tbl <- function(x) {

  old_attr <- attributes(x)
  if (any_is_date(x)) {
    date_attr <- vector("list", 2L)
    date_attr$date_col <- which_is_date(x)
    date_attr$series  <- dplyr::select(x, date_attr$date_col)
  } else {
    date_attr <- NULL
  }

  x <- dplyr::select(x, where(is.double) & where(is.numeric))

  tibble::new_tibble(x = x,
                     date_info = date_attr,
                     nrow = vctrs::vec_size(x),
                     class = "ffp_tbl")
}

#' @rdname ffp_tbl
#' @keywords internal
is_ffp_tbl <- function(x) inherits(x, "ffp_tbl")

#' @rdname ffp_tbl
#' @keywords internal
as_ffp_tbl <- function(x) new_ffp_tbl(x)

# flex_mat ----------------------------------------------------------------

# constructor
#' Internal class ffp_mat
#'
#' @rdname ffp_mat
#' @keywords internal
new_ffp_mat <- function(x) {
  validate_ffp_mat(ffp_mat(x))
}

# validator
#' @rdname ffp_mat
#' @keywords internal
validate_ffp_mat <- function(x) {
  stopifnot(is.numeric(x) & is.double(x))
  x
}

# helper
#' @rdname ffp_mat
#' @keywords internal
ffp_mat <- function(x, ...) {
  stopifnot(inherits(x, "ffp_tbl"))
  out <- as.matrix(x)
  out <- structure(out, class = "ffp_mat")
  out
}

#' @rdname ffp_mat
#' @keywords internal
is_ffp_mat <- function(x) inherits(x, "ffp_mat")

#' @rdname ffp_mat
#' @keywords internal
as_ffp_mat <- function(x) ffp_mat(x)


