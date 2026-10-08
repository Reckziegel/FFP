#' Least Information Kernel-Smoothing
#'
#' This is an internal function that uses the kernel-smoothing approach to
#' compute posterior probabilities satisfying target first and second moments.
#'
#' @param Y A numeric matrix containing the scenario features.
#' @param y A numeric vector containing the target posterior means.
#' @param h2 A numeric covariance matrix. If `NULL`, only the posterior means
#'   are constrained.
#'
#' @return A numeric vector containing the posterior probabilities.
#'
#' @keywords internal
least_info_kernel <- function(Y, y, h2) {
  moment_entropy_probabilities(
    x = Y,
    target_mean = y,
    target_covariance = h2
  )
}


moment_entropy_probabilities <- function(
    x,
    target_mean,
    target_covariance = NULL,
    control = entropy_solver_control(),
    call = rlang::caller_env()
) {

  x <- as.matrix(x)
  n_scenarios <- nrow(x)
  n_features  <- ncol(x)

  target_mean <- as.double(target_mean)

  a_eq <- t(x)
  b_eq <- target_mean

  feature_labels <- colnames(x)

  if (is.null(feature_labels)) {
    feature_labels <- paste0("V", seq_len(n_features))
  }

  metadata_feature_1 <- feature_labels
  metadata_feature_2 <- rep(NA_character_, n_features)
  metadata_labels <- paste0("Mean[", feature_labels, "]")

  if (!is.null(target_covariance)) {
    target_covariance <- as.matrix(target_covariance)
    pairs             <- matrix_constraint_pairs(n_features)
    second_moment     <- (target_covariance + tcrossprod(target_mean))

    second_moment_constraints <- second_moment_constraint_matrix(feature_values = x, pairs = pairs)
    second_moment_targets <- matrix_target_entries(target = second_moment, pairs = pairs)

    a_eq <- rbind(a_eq, second_moment_constraints)
    b_eq <- c(b_eq, second_moment_targets)

    first_labels  <- feature_labels[pairs[, "row"]]
    second_labels <- feature_labels[pairs[, "col"]]

    metadata_feature_1 <- c(metadata_feature_1, first_labels)
    metadata_feature_2 <- c(metadata_feature_2, second_labels)

    metadata_labels <- c(metadata_labels, paste0("Second moment[", first_labels, ", ", second_labels, "]"))

  }

  constraint_metadata <- new_constraint_metadata(
    type = "equality",
    method = "moment",
    feature_1 = metadata_feature_1,
    feature_2 = metadata_feature_2,
    label = metadata_labels
  )

  a_eq <- rbind(a_eq, rep(1, n_scenarios))

  storage.mode(a_eq) <- "double"
  dimnames(a_eq) <- NULL

  b_eq <- c(b_eq, 1)
  prior <- rep(1 / n_scenarios, n_scenarios)

  metadata <- new_entropy_problem_metadata(constraint_metadata = constraint_metadata, normalization_row = nrow(a_eq))

  problem <- new_ffp_entropy_problem(
    n_scenarios = n_scenarios,
    prior = prior,
    a_eq = a_eq,
    b_eq = b_eq,
    a_ineq = empty_constraint_matrix(n_scenarios),
    b_ineq = numeric(),
    metadata = metadata
  )

  solution <- solve_entropy_problem(problem = problem, control = control, call = call)

  as.double(solution$posterior)
}
