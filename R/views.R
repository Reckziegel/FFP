
# Views on Means ----------------------------------------------------------

#' Views on the Location Parameter
#'
#' Create views on the market the with the `view_on_*` functions.
#'
#' @param x An univariate or a multivariate dataset.
#' @param mean A \code{double} for the location paratemer of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
view_on_mean <- function(x, mean) {
  UseMethod("view_on_mean", x)
}

#' @rdname view_on_mean
#' @export
view_on_mean.default <- function(x, mean) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_mean
#' @export
view_on_mean.numeric <- function(x, mean) {
  construct_constraint_on_mean(x, mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.matrix <- function(x, mean) {
  construct_constraint_on_mean(x, mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.ts <- function(x, mean) {
  construct_constraint_on_mean(as.matrix(x), mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.xts <- function(x, mean) {
  construct_constraint_on_mean(as.matrix(x), mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.tbl_df <- function(x, mean) {
  construct_constraint_on_mean(tbl_to_mtx(x), mean)
}

#' @keywords internal
construct_constraint_on_mean <- function(x, mean) {
  assertthat::assert_that(assertthat::are_equal(NCOL(x), vctrs::vec_size(mean)))
  vctrs::vec_assert(mean, double())

  # ...constrain the first moments...
  Aeq <- t(x)
  beq <- mean

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}


# Views on Covariance -----------------------------------------------------

view_on_covariance <- function(x, mean, sigma) {
  UseMethod("view_on_covariance", x)
}

view_on_covariance.default <- function(x, mean, sigma) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.numeric <- function(x, mean) {
  construct_constraint_on_mean(x, mean)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.matrix <- function(x, mean) {
  construct_constraint_on_mean(x, mean)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.ts <- function(x, mean) {
  construct_constraint_on_mean(as.matrix(x), mean)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.xts <- function(x, mean) {
  construct_constraint_on_mean(as.matrix(x), mean)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.tbl_df <- function(x, mean) {
  construct_constraint_on_mean(tbl_to_mtx(x), mean)
}

construct_constraint_on_covariance <- function(x, mean, sigma) {

  N  <- NCOL(x)

  SecMom <- sigma + mean %*% t(mean)   #...constrain the second moments...

  Aeq <- NULL
  beq <- NULL

  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq, t(x[ , k] * x[ , l]))
      beq <- rbind(beq, SecMom[k, l])
    }
  }

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}





