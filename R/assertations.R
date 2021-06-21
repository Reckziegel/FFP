
assert_is_equal_size <- function(x, y) {
  assertthat::assert_that(
    vctrs::vec_size(x) == vctrs::vec_size(y)
    )
}

assert_is_logical <- function(x) {
  if (tibble::is_tibble(x)) {
    vctrs::vec_assert(x[[1]], logical())
  } else if (is.null(dim(x))) {
    vctrs::vec_assert(x, logical())
  } else {
    stop(deparse(x), " must be logical.", call. = FALSE)
  }
}

assert_is_univariate <- function(x) {
  if (NCOL(x) > 1) {
    stop("The conditioning variable (x) should be univariate.", call. = FALSE)
  }
}


assert_is_probability <- function(x) {
  inhe_num <- inherits(x, "numeric")
  inhe_ffp <- inherits(x, "ffp")
  if (!(inhe_num | inhe_ffp)) {
    stop("`p` must be numeric or an object of the ffp class.", call. = FALSE)
  }
  if (inhe_num) {
    if (sum(x) > 1.001) {
      stop("Probabilities can't exceed 1.", call. = FALSE)
    }
    if (any(x < 0)) {
      stop("Probabilities can't be negative")
    }
  }
}


# assertetion - fi_kernel -------------------------------------------------

assert_kernel_mean <- function(mean) {
  if (!(is.double(mean) && vctrs::vec_size(mean) == 1)) {
    stop("mean is not a number (a length one numeric vector).", call. = FALSE)
  }
}

assert_kernel_sigma <- function(sigma) {
  if (!(is.double(sigma) && vctrs::vec_size(sigma) == 1)) {
    stop("sigma is not a number (a length one numeric vector).", call. = FALSE)
  }
}

