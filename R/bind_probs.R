#' Stack Flexible-Probabilities
#'
#' This function mimics `dplyr` \code{\link[dplyr]{bind}}. It's useful if you
#' have different `ffp` objects and want to stack them in the `tidy` (long) format.
#'
#' @param ... Different \code{tibble} objects with the `ffp` signature to combine.
#'
#' @return A tidy \code{tibble}.
#'
#' @seealso \code{\link{crisp}} \code{\link{smoothing}} \code{\link{kernel_normal}}
#' \code{\link{kernel_entropy}} \code{\link{double_decay}}
#'
#' @export
#'
#' @examples
#' x <- smoothing(EuStockMarkets, 0.001)
#' y <- smoothing(EuStockMarkets, 0.002)
#' bind_probs(x, y)
bind_probs <- function(...) {

 dots <- rlang::list2(...)
 dots <- purrr::keep(dots, inherits, "ffp")

 # TODO add a warning when a list is discarded.

 if (is_empty(dots)) {
   stop("objects to bind must be of the `ffp` class.", call. = FALSE)
 }
 unique_rows <- unique(purrr::map_dbl(dots, vctrs::vec_size))
 if (length(unique_rows) > 1) {
   stop("Arguments to bind must have the same size (rows).", call. = FALSE)
 }

 seq_to_add  <- rep(1:length(dots), each = unique_rows)
 #types <- purrr::map(dots, attributes) %>% purrr::map_chr("type")
 #type_to_add <- rep(types, each = unique_rows)

 dplyr::bind_rows(dots) %>%
   dplyr::mutate(.key = as.factor(seq_to_add))
                 #.type = as.factor(type_to_add))

}



