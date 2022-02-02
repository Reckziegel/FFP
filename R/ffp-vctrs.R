#' Manipulate the `ffp` Class
#'
#' Helpers and Constructors from `ffp`.
#'
#' The `ffp` class is designed to interact with doubles,
#' but the output of `c(ffp, double)` or `c(double, ffp)` will always return
#' a `double` (not an `ffp` object), since there is no way to guarantee the
#' interaction between a numeric vector and a probability will also be a probability.
#'
#' @param x
#' \itemize{
#'   \item For `ffp()`: A numeric vector.
#'   \item For `is_ffp()`: An object to be tested.
#'   \item For `as_ffp()`: An object to convert to `ffp`.
#' }
#' @param ... Additional attributes to be passed to `ffp`.
#'
#' @return
#' \itemize{
#'   \item `ffp()` and `as_ffp()` return an S3 vector of class `ffp`
#'   (built upon \code{double}'s);
#'   \item `is_ffp()` returns a \code{logical} object.
#' }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- runif(5)
#' p <- p / sum(p)
#'
#' is_ffp(p)
#' ffp(p)
ffp <- function(x = double(), ...) {
  vctrs::vec_cast(x, double())
  new_ffp(x, ...)
}

#' @rdname ffp
#' @export
is_ffp <- function(x) {
  inherits(x, "ffp")
}

#' @rdname ffp
#' @export
as_ffp <- function(x) {
  UseMethod("as_ffp", x)
}

#' @rdname ffp
#' @export
as_ffp.default <- function(x) {
  assertthat::assert_that(dplyr::near(sum(x), 1), msg = "Probabilities must sum 1.")
  assertthat::assert_that(all(x >= 0), msg = "Probabilities can't be negative.")
  vctrs::vec_cast(x, new_ffp())
}

#' @rdname ffp
#' @export
as_ffp.integer <- function(x) {
  assertthat::assert_that(dplyr::near(sum(x), 1), msg = "Probabilities must sum 1.")
  assertthat::assert_that(all(x >= 0), msg = "Probabilities can't be negative.")
  vctrs::vec_cast(as.double(x), new_ffp())
}


#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @import vctrs
#' @keywords internal
#' @name ffp-vctrs
NULL

# for compatibility with the S4 system
methods::setOldClass(c("ffp", "vctrs_vctr"))

#' @rdname ffp-vctrs
#' @export
new_ffp <- function(x = double(), ...) {
  vctrs::vec_assert(x, double())
  vctrs::new_vctr(x, class = "ffp", ...)
}

#' @rdname ffp-vctrs
#' @export
vec_ptype_abbr.ffp <- function(x, ...) "ffp"

#' @rdname ffp-vctrs
#' @export
vec_ptype2.ffp.ffp <- function(x, y, ...) new_ffp()

#' @rdname ffp-vctrs
#' @export
vec_ptype2.ffp.double <- function(x, y, ...) double()

#' @rdname ffp-vctrs
#' @export
vec_ptype2.double.ffp <- function(x, y, ...) double()

#' @rdname ffp-vctrs
#' @export
vec_cast.ffp.ffp <- function(x, to, ...) x

#' @rdname ffp-vctrs
#' @export
vec_cast.ffp.double <- function(x, to, ...) ffp(x)

#' @rdname ffp-vctrs
#' @export
vec_cast.double.ffp <- function(x, to, ...) vctrs::vec_data(x)

#' @rdname ffp-vctrs
#' @export
obj_print_data.ffp <- function(x, ...) {
  if (vctrs::vec_size(x) <= 5) {
    cat(x)
  } else {
    cat(utils::head(x, 5), "...", utils::tail(x, 1))
  }
}

#' @rdname ffp-vctrs
#' @export
vec_math.ffp <- function(.fn, .x, ...) vctrs::vec_math_base(.fn, .x, ...)

#' @rdname ffp-vctrs
#' @export
vec_arith.ffp <- function(op, x, y, ...) vctrs::vec_arith_base(op, x, y, ...)

