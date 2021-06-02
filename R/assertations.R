is_equal_size <- function(x, y) {
  assertthat::assert_that(
    vctrs::vec_size(x) == vctrs::vec_size(y)
    )
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

assert_is_univariate <- function(x) {
  if (NCOL(x) > 1) {
    stop("The conditioning variable (x) should be univariate.", call. = FALSE)
  }
}
