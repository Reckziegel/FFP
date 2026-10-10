#' @keywords internal
any_is_date <- function(x) {
  if (!(tibble::is_tibble(x) | is.data.frame(x))) cli::cli_abort("Argument {.arg x} must be a tibble or a data.frame")
  purrr::some(x, lubridate::is.Date)
}

#' @keywords internal
any_is_double <- function(x) {
  if (!tibble::is_tibble(x)) cli::cli_abort("Argument {.arg x} must be a tibble")
  any(purrr::map_lgl(x, ~ is.double(.) && is.numeric(.)))
}

#' @keywords internal
is_empty <- function(x) length(x) == 0

#' @keywords internal
which_is_date <- function(x) {
  if (!tibble::is_tibble(x)) cli::cli_abort("Argument {.arg x} must be a tibble")
  purrr::detect_index(x, methods::is, "Date")
}

#' @keywords internal
get_date_col <- function(x) {
  if (!tibble::is_tibble(x)) cli::cli_abort("Argument {.arg x} must be a tibble")
  dplyr::select(x, where(lubridate::is.Date))
}

#' @keywords internal
get_double_col <- function(x) {
  if (!tibble::is_tibble(x)) cli::cli_abort("Argument {.arg x} must be a tibble")
  dplyr::select(x, where(is.double) & where(is.numeric))
}

#' @keywords internal
has_dim <- function(x) !is.null(dim(x))

#' @keywords internal
ffp_match_call <- function(x, ...) rlang::expr(!!match.call())

#' @keywords internal
tbl_to_mtx <- function(x) as.matrix(dplyr::select(x, where(is.numeric)))


# Check generic input -----------------------------------------------------

#' Internal function used to check the validity of inputs.
#'
#' @param x Any object passed to other functions in the package.
#'
#' @return A matrix
#'
#' @keywords internal
check_input <- function(x) {
  UseMethod("check_input", x)
}

#' @rdname check_input
#' @exportS3Method
check_input.default <- function(x) {
  stop("`ffp` doesn't support the `", class(x)[[1L]], "` yet.", call. = FALSE)
}

#' @rdname check_input
check_input.numeric <- function(x) as.matrix(x)
#' @exportS3Method

#' @rdname check_input
check_input.double <- function(x) as.matrix(x)
#' @exportS3Method

#' @rdname check_input
check_input.matrix <- function(x) x
#' @exportS3Method

#' @rdname check_input
check_input.xts <- function(x) as.matrix(x)
#' @exportS3Method

#' @rdname check_input
check_input.tbl_df <- function(x) tbl_to_mtx(x)
#' @exportS3Method



