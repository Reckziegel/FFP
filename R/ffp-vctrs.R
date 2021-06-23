#' Fully Flexible Probability Class
#'
#' Functions to help the user to manipulate the `ffp` class.
#'
#' @param x
#' \itemize{
#'   \item For `ffp()`: A numeric vector (must be a probability)
#'   \item For `is_ffp()`: An object to test if the class is `ffp`
#'   \item For `as_ffp()`: An object to convert to `ffp`
#' }
#'
#' @return
#' \itemize{
#'   \item `ffp()` and `as_ffp()` return an S3 vector of class `ffp`
#'   \item `is_ffp()` returns a \code{logical} object.
#' }
#'
#'
#' @export
#' @examples
#' set.seed(123)
#' p <- runif(5)
#' p <- p / sum(p)
#' ffp(p)
#' is_ffp(p)
ffp <- function(x = double()) {
  vctrs::vec_cast(x, double())
  new_ffp(x)
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
  vctrs::vec_cast(x, new_ffp())
}

#' @rdname ffp
#' @export
as_ffp.integer <- function(x) {
  vctrs::vec_cast(as.double(x), new_ffp())
}


#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name ffp-vctrs
NULL

# for compatibility with the S4 system
methods::setOldClass(c("ffp", "vctrs_vctr"))

#' @rdname ffp-vctrs
#' @export
new_ffp <- function(x = double()) {
  vctrs::vec_assert(x, double())
  vctrs::new_vctr(x, class = "ffp")
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

# flex_mat ----------------------------------------------------------------

# constructor
#' Internal class ffp_mat
#'
#' @rdname ffp_mat
#' @export
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


