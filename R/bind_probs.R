#' Stack Flexible Probabilities
#'
#' This function mimics `dplyr` \code{\link[dplyr]{bind}}. It's useful if you
#' have different `ffp` objects and want to stack them in the `tidy` (long) format.
#'
#' @param ... \code{ffp} objects to combine.
#'
#' @return A tidy \code{tibble} with numeric probability values suitable for visualization
#' and tabular analysis.
#'
#' The output contains three columns:
#' \itemize{
#'   \item `rowid` (an \code{integer}) with the row number of each realization;
#'   \item `probs` (a \code{numeric} vector) with probability weights extracted from the `ffp` objects;
#'   \item `fn` (a \code{factor}) that keeps track of which `ffp` input each row comes from.
#' }
#'
#' @details
#' The `probs` column is extracted as numeric data to enable direct use with ggplot2 and
#' other tabular visualization and analysis tools. Once `ffp` objects are reshaped to tidy format,
#' they cease to represent a single probability distribution and instead represent structured
#' tabular data where the numeric probability values are ordinary columns.
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
#' bind_probs(x, y) |>
#'   ggplot(aes(x = rowid, y = probs, color = fn)) +
#'   geom_line() +
#'   scale_color_viridis_d() +
#'   theme(legend.position="bottom")
bind_probs <- function(...) {

    dots <- rlang::list2(...)
    dots <- purrr::keep(dots, inherits, "ffp")

    # TODO add a warning when a list is discarded.

    if (is_empty(dots)) {
        cli::cli_abort("objects to bind must be of class {.cls ffp}.")
    }

    unique_rows <- unique(purrr::map_dbl(dots, vctrs::vec_size))
    if (length(unique_rows) > 1) {
        cli::cli_abort(c(
            "Arguments to bind must have the same size (rows).",
            "x" = "Got {length(unique_rows)} different sizes: {.val {unique_rows}}"
        ))
    }

    # attr_fn   <- purrr::map(purrr::map(dots, attributes), 1)
    # attr_list <- purrr::map(purrr::map(dots, attributes), 2) |> purrr::map(as.list)
    # attr_nms  <- purrr::map(attr_list, names) |> purrr::map(3)
    # attr_vl   <- purrr::map(attr_list, 3)
    # fn <- paste0(as.character(attr_fn), ": ", as.character(attr_nms), " = ", as.vector(attr_vl))
    fn <- as.character(purrr::map(purrr::map(dots, attributes), "user_call"))
    fn <- stringr::str_remove(fn, ".numeric") |>
        stringr::str_remove(pattern = ".matrix") |>
        stringr::str_remove(pattern = ".ts") |>
        stringr::str_remove(pattern = ".xts") |>
        stringr::str_remove(pattern = ".tbl_df") |>
        stringr::str_remove(pattern = ".data.frame")
    fn <- rep(fn, each = unique_rows)

    seq_to_add  <- rep(1:length(dots), each = unique_rows)

    purrr::map(dots, tibble::as_tibble) |>
        purrr::map(tibble::rowid_to_column) |>
        dplyr::bind_rows() |>
        dplyr::rename(probs = "value") |>
        dplyr::mutate(
            probs = vctrs::vec_data(probs),
            fn = as.factor(fn)
        )

}




