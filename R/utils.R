#' @keywords internal
any_is_date <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  purrr::some(x, lubridate::is.Date)
}

#' @keywords internal
any_is_double <- function(x) {
  stopifnot(tibble::is_tibble(x))
  any(purrr::map_lgl(x, ~ is.double(.) && is.numeric(.)))
}

#' @keywords internal
is_empty <- function(x) length(x) == 0

#' @keywords internal
which_is_date <- function(x) {
  stopifnot(tibble::is_tibble(x))
  purrr::detect_index(x, methods::is, "Date")
}

#' @keywords internal
get_date_col <- function(x) {
  stopifnot(tibble::is_tibble(x))
  dplyr::select(x, where(lubridate::is.Date))
}

#' @keywords internal
get_double_col <- function(x) {
  stopifnot(tibble::is_tibble(x))
  dplyr::select(x, where(is.double) & where(is.numeric))
}

#' @keywords internal
has_dim <- function(x) !is.null(dim(x))

#' @keywords internal
ffp_match_call <- function(x, ...) rlang::expr(!!match.call())

#' @keywords internal
tbl_to_mtx <- function(x) as.matrix(dplyr::select(x, where(is.numeric)))

#' @keywords internal
histc <- function(x, cuts) {

  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(cuts))

  cuts <- c(cuts)
  n <- length(cuts)
  if (is.unsorted(cuts)) stop("Argument `cuts` must be a monotonically non-decreasing vector.")

  if (length(cuts) == 1) {
    bin <- numeric(length(x))
    if (is.matrix(x)) dim(bin) <- c(n, ncol(x))
    list(cnt = 0, bin = bin)
  }

  bin <- numeric(length(x))
  if (is.vector(x)) {
    cnt <- numeric(n)
    for (i in 1:(n - 1)) {
      li <- cuts[i] <= x & x < cuts[i + 1]
      cnt[i] <- sum(li)
      bin[li] <- i
    }
    li <- x == cuts[n]
    cnt[n] <- sum(li)
    bin[li] <- n

  } else if (is.matrix(x)) {
    cnt <- matrix(0, n, ncol(x))
    for (i in 1:(n - 1)) {
      li <- cuts[i] <= x & x < cuts[i + 1]
      cnt[i, ] <- apply(li, 2, sum)
      bin[li] <- i
    }
    li <- x == cuts[n]
    cnt[n, ] <- apply(li, 2, sum)
    bin[li] <- n

  } else {
    stop("Argument `x` must be a numeric vector or matrix.")
  }

  dim(bin) <- dim(x)
  list(cnt = cnt, bin = bin)
}


