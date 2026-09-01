#' Manipulate the `ffp` Class
#'
#' Helpers and constructors for flexible probabilities.
#'
#' An `ffp` object represents a complete probability distribution. Values must
#' be non-negative and sum to one.
#'
#' Operations that cannot guarantee preservation of the probability invariant
#' either return a plain `double` or are explicitly rejected.
#'
#' @param x
#' \itemize{
#'   \item For `ffp()`: A numeric vector representing a probability
#'     distribution. Values must be non-negative and sum to 1
#'     (with tolerance 0.001).
#'   \item For `is_ffp()`: An object to be tested.
#'   \item For `as_ffp()`: An object to convert to `ffp`.
#' }
#' @param ... Additional attributes passed to `new_ffp()`.
#'
#' @return
#' \itemize{
#'   \item `ffp()` and `as_ffp()` return an S3 vector of class `ffp`.
#'   \item `is_ffp()` returns a logical value.
#' }
#'
#' @details
#' Validation levels:
#'
#' - `ffp()`: practical validation with tolerance `0.001`.
#' - `as_ffp()`: strict validation using approximately machine precision.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- runif(5)
#' p <- p / sum(p)
#'
#' is_ffp(p)
#' as_ffp(p)
ffp <- function(x = double(), ...) {
  x <- vctrs::vec_cast(x, double())
  validate_probabilities(x, tolerance = 0.001)
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
  x <- vctrs::vec_cast(x, double())
  tolerance <- sqrt(.Machine$double.eps)
  validate_probabilities(x, tolerance = tolerance)
  new_ffp(x)
}


#' @rdname ffp
#' @export
as_ffp.integer <- function(x) {
  as_ffp(as.double(x))
}


#' @rdname ffp
#' @export
as_ffp.ffp <- function(x) {
  x
}


#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @importFrom vctrs new_vctr vec_assert vec_size
#' @keywords internal
#' @name ffp-vctrs
NULL


# Compatibility with the S4 system.
methods::setOldClass(c("ffp", "vctrs_vctr"))


#' @rdname ffp-vctrs
#' @export
new_ffp <- function(x = double(), ...) {
  vctrs::vec_assert(x, double())
  vctrs::new_vctr(x, class = "ffp",...)
}


#' @rdname ffp-vctrs
#' @export
vec_ptype_abbr.ffp <- function(x, ...) {
  "ffp"
}


# Concatenating probability distributions does not generally produce
# another probability distribution.
#' @rdname ffp-vctrs
#' @export
vec_ptype2.ffp.ffp <- function(x, y, ...) {
  double()
}


#' @rdname ffp-vctrs
#' @export
vec_ptype2.ffp.double <- function(x, y, ...) {
  double()
}


#' @rdname ffp-vctrs
#' @export
vec_ptype2.double.ffp <- function(x, y, ...) {
  double()
}


#' @rdname ffp-vctrs
#' @export
vec_cast.ffp.ffp <- function(x, to, ...) {
  x
}


#' @rdname ffp-vctrs
#' @export
vec_cast.ffp.double <- function(x, to, ...) {
  tolerance <- sqrt(.Machine$double.eps)
  validate_probabilities(x, tolerance = tolerance)
  new_ffp(x)
}


#' @rdname ffp-vctrs
#' @export
vec_cast.double.ffp <- function(x, to, ...) {
  vctrs::vec_data(x)
}


# Subsetting cannot guarantee preservation of the probability invariant.
#' @export
`[.ffp` <- function(x, i, ...) {
  vctrs::vec_data(x)[i]
}


#' @export
`[[.ffp` <- function(x, i, ...) {
  vctrs::vec_data(x)[[i]]
}


# Repetition changes total probability mass.
#' @export
rep.ffp <- function(x, ...) {
  rep(vctrs::vec_data(x), ...)
}


# Direct mutation could invalidate the probability distribution.
#' @export
`[<-.ffp` <- function(x, i, value) {
  cli::cli_abort(
    c(
      "Direct modification of {.cls ffp} vectors is not allowed.",
      "!" = "This preserves the probability distribution invariant.",
      "i" = "Create a new probability vector and convert it with {.fn ffp}."
    ),
    class = "ffp_error_assignment"
  )
}


#' @export
`[[<-.ffp` <- function(x, i, value) {
  cli::cli_abort(
    c(
      "Direct modification of {.cls ffp} vectors is not allowed.",
      "!" = "This preserves the probability distribution invariant.",
      "i" = "Create a new probability vector and convert it with {.fn ffp}."
    ),
    class = "ffp_error_assignment"
  )
}


#' @rdname ffp-vctrs
#' @export
obj_print_data.ffp <- function(x, ...) {
  data <- vctrs::vec_data(x)

  if (vctrs::vec_size(x) <= 5) {
    cat(data)
  } else {
    cat(utils::head(data, 5), "...", utils::tail(data, 1))
  }
}


#' @rdname ffp-vctrs
#' @export
vec_math.ffp <- function(.fn, .x, ...) {
  vctrs::vec_math_base(.fn, .x, ...)
}


# Arithmetic ---------------------------------------------------------------

abort_ffp_arithmetic <- function() {
  cli::cli_abort(
    c(
      "Arithmetic operations on {.cls ffp} vectors are not allowed.",
      "!" = "This preserves the probability distribution invariant.",
      "i" = paste0(
        "For valid combinations, use {.fn average_ffp} ",
        "or {.fn combine_ffp}."
      )
    ),
    class = "ffp_error_arithmetic"
  )
}


#' @rdname ffp-vctrs
#' @export
#' @method vec_arith ffp
#' @importFrom vctrs vec_arith
vec_arith.ffp <- function(op, x, y, ...) {
  abort_ffp_arithmetic()
}


#' @rdname ffp-vctrs
#' @export
#' @method vec_arith.numeric ffp
#' @importFrom vctrs vec_arith.numeric
vec_arith.numeric.ffp <- function(op, x, y, ...) {
  abort_ffp_arithmetic()
}
