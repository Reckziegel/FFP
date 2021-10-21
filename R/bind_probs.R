#' Stack Flexible Probabilities
#'
#' This function mimics `dplyr` \code{\link[dplyr]{bind}}. It's useful if you
#' have different `ffp` objects and want to stack them in the `tidy` (long) format.
#'
#' @param ... \code{ffp} objects to combine.
#'
#' @return A tidy \code{tibble}.
#'
#' The output adds two new columns:
#' \itemize{
#'   \item `rowid` (an \code{integer}) with the row number of each realization;
#'   \item `key` (a \code{factor}) that keeps track of the `ffp` inputs as separated objects.
#' }
#'
#' @seealso \code{\link{crisp}} \code{\link{exp_decay}} \code{\link{kernel_normal}}
#' \code{\link{kernel_entropy}} \code{\link{double_decay}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' x <- exp_decay(EuStockMarkets, lambda = 0.001)
#' y <- exp_decay(EuStockMarkets, lambda = 0.002)
#'
#' bind_probs(x, y)
#'
#' bind_probs(x, y) %>%
#'   ggplot(aes(x = rowid, y = probs, color = fn)) +
#'   geom_line() +
#'   scale_color_viridis_d()
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

 attr_fn   <- purrr::map(purrr::map(dots, attributes), 1)
 attr_list <- purrr::map(purrr::map(dots, attributes), 2) %>% purrr::map(as.list)
 attr_nms  <- purrr::map(attr_list, names) %>% purrr::map(3)
 attr_vl   <- purrr::map(attr_list, 3)
 fn <- paste0(as.character(attr_fn), ": ", as.character(attr_nms), " = ", as.vector(attr_vl))
 fn <- rep(fn, each = unique_rows)

 seq_to_add  <- rep(1:length(dots), each = unique_rows)

 purrr::map(dots, tibble::as_tibble) %>%
    purrr::map(tibble::rowid_to_column) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(probs = "value") %>%
    dplyr::mutate(fn = as.factor(fn))

}



