# View specifications -----------------------------------------------------

new_view_spec <- function(method, x, target, parameters = list(), class) {

  structure(
    list(
      method = method,
      x = x,
      target = target,
      parameters = parameters
    ),
    class = c(class, "ffp_view_spec")
  )
}


#' Mean view
#'
#' Creates a view specification on posterior means.
#'
#' `x` can be a scenario variable, an expression evaluated from the scenario
#' data, an external vector or panel. External features are matched to model
#' scenarios strictly by position. The package does not reorder, join, recycle,
#' or otherwise align observations automatically.
#'
#' If `x` resolves to multiple features, `target` must contain exactly one value
#' per feature. Targets are matched to features by position and are never
#' recycled automatically.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression. It may resolve to a vector or a rectangular panel
#'   with one row per scenario.
#' @param target A finite numeric vector containing exactly one posterior mean
#'   target per feature in `x`.
#'
#' @return An object of class `ffp_view_mean_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_probability()], [view_quantile()],
#'   [view_volatility()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.04, -0.01, 0.02, 0.05),
#'   rates = c(0.01, 0.015, 0.02, 0.025)
#' )
#'
#' # A view on a scenario variable
#' view_mean(
#'   x = equity,
#'   target = 0.01
#' )
#'
#' # A view on a transformation
#' view_mean(
#'   x = equity - rates,
#'   target = 0
#' )
#'
#' # Multiple features require one target per feature
#' ret <- diff(log(EuStockMarkets))
#'
#' view_mean(
#'   x = ret,
#'   target = rep(0.02, NCOL(ret))
#' )
view_mean <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_mean} requires {.arg target}.",
        "i" = "Supply exactly one posterior mean target per feature in {.arg x}."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_mean",call = call)
  target <- validate_mean_target(target = target, call = call)

  new_view_spec(
    method = "mean",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_mean_spec"
  )
}


#' Probability view
#'
#' Creates a view specification on posterior event probabilities.
#'
#' `x` must resolve to one or more logical features, with each feature defining
#' one event scenario by scenario. Logical expressions can therefore describe
#' marginal, joint, union, interval, and other events directly using ordinary R
#' operators such as `&`, `|`, `!`, and `%in%`.
#'
#' When `given` is supplied, the view is interpreted as a conditional
#' probability. `given` must resolve to exactly one logical event and that same
#' conditioning event applies to every event represented by `x`. The
#' conditioning event must have positive probability under the model prior when
#' the view is attached with [ffp_view()].
#'
#' External event features are matched to model scenarios strictly by position.
#' The package does not reorder, join, recycle, or otherwise align observations
#' automatically.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A logical scenario
#'   feature or expression defining one or more events. It may resolve to a
#'   logical vector or a rectangular logical panel with one row per scenario.
#' @param target A numeric vector in `[0, 1]` containing exactly one posterior
#'   probability target per event in `x`.
#' @param given <[`data-masking`][rlang::args_data_masking]> An optional logical
#'   scenario feature or expression defining exactly one conditioning event. If
#'   `NULL`, the probability view is unconditional.
#'
#' @return An object of class `ffp_view_probability_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2012). Stress-Testing with Fully Flexible Causal Inputs.
#'
#' @seealso [ffp_view()], [view_mean()], [view_quantile()], [view_volatility()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
#'   recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' )
#'
#' # Marginal event probability
#' view_probability(
#'   x = inflation > 0.04,
#'   target = 0.25
#' )
#'
#' # Conditional probability
#' view_probability(
#'   x = equity < 0,
#'   target = 0.70,
#'   given = recession
#' )
#'
#' # One conditioning event can apply to several explicitly targeted events
#' event_panel <- cbind(
#'   loss = scenarios$equity < 0,
#'   severe_loss = scenarios$equity < -0.05
#' )
#'
#' view_probability(
#'   x = event_panel,
#'   target = c(0.70, 0.20),
#'   given = scenarios$recession
#' )
view_probability <- function(x, target, given = NULL) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_probability} requires {.arg target}.",
        "i" = "Supply exactly one probability target per event in {.arg x}."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x     <- rlang::enquo(x)
  given <- rlang::enquo(given)

  validate_view_x_spec(x = x, constructor = "view_probability", call = call)
  target <- validate_probability_target(target = target, call = call)

  if (rlang::quo_is_null(given) || rlang::quo_is_missing(given)) {
    given <- NULL
  }

  new_view_spec(
    method = "probability",
    x = x,
    target = target,
    parameters = list(given = given),
    class = "ffp_view_probability_spec"
  )
}


#' Quantile view
#'
#' Creates an equality view on posterior quantiles.
#'
#' `x` identifies the scenario feature, `target` gives the desired quantile
#' threshold, and `level` specifies which posterior quantile is being viewed.
#'
#' @section Discrete quantile definition:
#'
#' FFP represents a distribution through fixed scenarios and their
#' probabilities. Quantile views therefore use the quantile definition for a
#' discrete distribution directly; they do not interpolate between scenario
#' values and do not use any of the interpolation types from
#' [stats::quantile()].
#'
#' For a posterior probability vector \eqn{\tilde p}, quantile level \eqn{u},
#' and target \eqn{q}, the view means that \eqn{q} belongs to the set of
#' posterior \eqn{u}-quantiles. Equivalently,
#'
#' \deqn{
#'   P_{\tilde p}(X < q) \le u
#'   \quad\text{and}\quad
#'   P_{\tilde p}(X > q) \le 1-u.
#' }
#'
#' In terms of the posterior cumulative distribution function \eqn{F_{\tilde p}},
#' this can also be written as
#'
#' \deqn{
#'   F_{\tilde p}(q^-) \le u \le F_{\tilde p}(q).
#' }
#'
#' This definition handles probability mass exactly at `target` naturally. In
#' particular, the posterior does not need to place a scenario exactly at an
#' interpolated sample quantile.
#'
#' @section Multiple features and levels:
#'
#' If `x` resolves to `K` features, `target` must contain exactly `K` values.
#' Targets are matched to features by position and are never recycled.
#'
#' `level` may contain either one value, meaning that all features use the same
#' quantile level, or exactly one value per target. A common scalar level is a
#' property of the statistic being requested; it does not create additional
#' target views implicitly.
#'
#' Each quantile target creates two mathematical inequality constraints: one
#' below `target` and one above it. A single vectorized call is still stored as
#' one semantic view.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression. It may resolve to a vector or a rectangular panel
#'   with one row per scenario.
#' @param target A finite numeric vector containing exactly one quantile target
#'   per feature in `x`.
#' @param level A numeric quantile level strictly between 0 and 1. Supply
#'   either one common level for all targets or exactly one level per target.
#'
#' @return An object of class `ffp_view_quantile_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_mean()], [view_probability()],
#'   [view_volatility()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.12, -0.08, -0.03, 0.01, 0.04, 0.07),
#'   inflation = c(0.015, 0.020, 0.025, 0.035, 0.045, 0.060)
#' )
#'
#' # The posterior 5% quantile should be -8%.
#' view_quantile(
#'   x = equity,
#'   target = -0.08,
#'   level = 0.05
#' )
#'
#' # The posterior 95% quantile of inflation should be 6%.
#' view_quantile(
#'   x = inflation,
#'   target = 0.06,
#'   level = 0.95
#' )
#'
#' # The same quantile level can be applied to a panel. The targets remain
#' # explicit: one target is required for every feature.
#' ret <- diff(log(EuStockMarkets))
#'
#' view_quantile(
#'   x = ret,
#'   target = c(-0.08, -0.07, -0.09, -0.06),
#'   level = 0.05
#' )
#'
#' # Different features may use different quantile levels.
#' view_quantile(
#'   x = ret,
#'   target = c(-0.08, -0.05, -0.10, 0.07),
#'   level = c(0.05, 0.10, 0.01, 0.95)
#' )
#'
#' # Views can act on arbitrary numeric transformations.
#' view_quantile(
#'   x = abs(equity),
#'   target = 0.08,
#'   level = 0.90
#' )
#'
#' # Bind the specification to a model. The expression is evaluated only at
#' # this stage and the realized feature values are stored in the model.
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(prior_uniform()) |>
#'   ffp_view(
#'     view_quantile(
#'       x = equity,
#'       target = -0.08,
#'       level = 0.05
#'     )
#'   )
view_quantile <- function(x, target, level) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_quantile} requires {.arg target}.",
        "i" = "Supply exactly one quantile target per feature in {.arg x}."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  if (missing(level)) {
    ffp_abort(
      c(
        "{.fn view_quantile} requires {.arg level}.",
        "i" = "Supply a quantile level strictly between 0 and 1."
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_quantile", call = call)

  target <- validate_quantile_target(target = target, call = call)
  level  <- resolve_quantile_level(level = level, n_targets = length(target), call = call)

  new_view_spec(
    method = "quantile",
    x = x,
    target = target,
    parameters = list(level = level),
    class = "ffp_view_quantile_spec"
  )
}


#' Median view
#'
#' Creates a view specification on posterior medians.
#'
#' `view_median()` is the median-specific counterpart of [view_quantile()].
#' It uses the discrete quantile definition directly, with quantile level 0.5.
#' No interpolation between scenario values is performed.
#'
#' If `x` resolves to multiple features, `target` must contain exactly one
#' median target per feature. Targets are matched to features by position and
#' are never recycled automatically.
#'
#' @section Discrete median definition:
#'
#' For posterior probabilities \eqn{\tilde p} and median target \eqn{m}, the
#' equality view requires
#'
#' \deqn{
#'   P_{\tilde p}(X < m) \le 0.5
#'   \quad\text{and}\quad
#'   P_{\tilde p}(X > m) \le 0.5.
#' }
#'
#' Equivalently, \eqn{m} belongs to the set of posterior medians. Because the
#' scenario distribution is discrete, one median target generates two
#' mathematical constraints.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression. It may resolve to a vector or a rectangular panel
#'   with one row per scenario.
#' @param target A finite numeric vector containing exactly one median target
#'   per feature in `x`.
#'
#' @return An object of class `ffp_view_median_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_quantile()], [view_mean()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05)
#' )
#'
#' # A median view on one scenario variable
#' view_median(
#'   x = equity,
#'   target = 0.01
#' )
#'
#' # Views can act on arbitrary numeric transformations
#' view_median(
#'   x = equity - inflation,
#'   target = -0.02
#' )
#'
#' # Multiple features require one explicit target per feature
#' view_median(
#'   x = scenarios,
#'   target = c(0.01, 0.035)
#' )
view_median <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_median} requires {.arg target}.",
        "i" = "Supply exactly one median target per feature in {.arg x}."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_median", call = call)
  target <- validate_median_target(target = target, call = call)

  new_view_spec(
    method = "median",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_median_spec"
  )
}


#' Ranking view
#'
#' Creates a relative ranking view on posterior means.
#'
#' `view_rank()` states that explicitly selected features should be ordered by
#' posterior mean. The ranking order must always be supplied by the user. If
#' the selected features are \eqn{X_1, \ldots, X_K} in the requested order,
#' the view is
#'
#' \deqn{
#'   E_{\tilde p}[X_1] \ge E_{\tilde p}[X_2] \ge \cdots
#'   \ge E_{\tilde p}[X_K].
#' }
#'
#' The ranking is represented through adjacent differences, so `K` ranked
#' features generate `K - 1` linear constraints. For example,
#' \eqn{E[X_1] \ge E[X_2] \ge E[X_3]} becomes
#' \eqn{E[X_1 - X_2] \ge 0} and \eqn{E[X_2 - X_3] \ge 0}.
#'
#' The inequalities are weak inequalities. Ties are therefore admissible; the
#' function does not add an implicit numerical margin or epsilon.
#'
#' @section Choosing the ranking order:
#'
#' `order` is required. The user must state explicitly which features
#' participate in the ranking and in which order they are expected to perform.
#'
#' A character `order` selects and orders features by name. This requires named
#' features and may select only a subset of the available features.
#'
#' A numeric `order` selects and orders features by one-based column position.
#' Positions must be finite positive whole numbers and may also select a subset.
#'
#' In all cases, at least two distinct features must participate in the ranking.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression resolving to at least two features.
#' @param order Required ranking order. Supply a character vector of feature
#'   names or a numeric vector of one-based feature positions.
#'
#' @return An object of class `ffp_view_rank_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_mean()], [view_median()]
#'
#' @export
#'
#' @examples
#' returns <- cbind(
#'   DAX = c(0.01, -0.02, 0.03, 0.02),
#'   FTSE = c(0.00, -0.01, 0.02, 0.01),
#'   CAC = c(-0.01, 0.00, 0.01, 0.02),
#'   SP500 = c(0.02, -0.01, 0.01, 0.00)
#' )
#'
#' # Select and order features by name
#' view_rank(
#'   x = returns,
#'   order = c("DAX", "FTSE", "CAC")
#' )
#'
#' # Select and order features by position
#' view_rank(
#'   x = returns,
#'   order = c(1, 4, 2)
#' )
view_rank <- function(x, order) {

  call <- rlang::caller_env()

  if (missing(order)) {
    ffp_abort(
      c(
        "{.fn view_rank} requires {.arg order}.",
        "i" = paste0(
          "Explicitly supply at least two feature names or one-based ",
          "feature positions in the expected ranking order."
        )
      ),
      class = c(
        "ffp_error_invalid_view_order",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_rank", call = call)
  order <- validate_rank_order_spec(order = order, call = call)

  new_view_spec(
    method = "rank",
    x = x,
    target = NULL,
    parameters = list(order = order),
    class = "ffp_view_rank_spec"
  )
}


#' Volatility view
#'
#' Creates an equality view on posterior volatility using the linear Fully
#' Flexible Views formulation of Meucci (2008).
#'
#' `x` can be a scenario variable, an expression evaluated from the scenario
#' data, an external numeric vector or panel. External features are matched
#' to model scenarios strictly by position. The package does not reorder, join,
#' recycle, or otherwise align observations automatically.
#'
#' If `x` resolves to multiple features, `target` must contain exactly one
#' volatility target per feature. Targets are matched to features by position
#' and are never recycled automatically.
#'
#' @section Mathematical definition:
#'
#' For a scenario feature \eqn{X_k}, a volatility target \eqn{\sigma_k}, and
#' posterior probabilities \eqn{\tilde p}, the standard-deviation view in the
#' Fully Flexible Views formulation is represented by the linear constraint
#'
#' \deqn{
#'   \sum_{t=1}^{T} \tilde p_t X_{t,k}^2
#'   =
#'   \hat m_k^2 + \sigma_k^2,
#' }
#'
#' where \eqn{\hat m_k} is the reference/sample mean of the feature. This is the
#' formulation used by Meucci to keep volatility views linear in the posterior
#' scenario probabilities.
#'
#' The public `target` is expressed directly in standard-deviation units. The
#' reference mean is an internal quantity used when the view is later compiled
#' for Entropy Pooling and is not an additional user parameter.
#'
#' A volatility target must be non-negative. A target of zero is allowed.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression. It may resolve to a vector or a rectangular panel
#'   with one row per scenario.
#' @param target A finite, non-negative numeric vector containing exactly one
#'   volatility target per feature in `x`.
#'
#' @return An object of class `ffp_view_volatility_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_mean()], [view_probability()], [view_quantile()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   rates = c(-0.01, 0.005, 0.01, -0.005, 0.015)
#' )
#'
#' # A volatility view on one scenario variable
#' view_volatility(
#'   x = equity,
#'   target = 0.15
#' )
#'
#' # Views can act on arbitrary numeric transformations
#' view_volatility(
#'   x = equity - rates,
#'   target = 0.10
#' )
#'
#' # Multiple features require one explicit target per feature
#' view_volatility(
#'   x = scenarios,
#'   target = c(0.15, 0.05)
#' )
#'
#' # Bind the specification to an FFP model
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(prior_uniform()) |>
#'   ffp_view(view_volatility(x = equity, target = 0.15))
view_volatility <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_volatility} requires {.arg target}.",
        "i" = paste0("Supply exactly one volatility target per feature in {.arg x}.")
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_volatility", call = call)
  target <- validate_volatility_target(target = target, call = call)

  new_view_spec(
    method = "volatility",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_volatility_spec"
  )
}


# Attach views ------------------------------------------------------------

#' Add views to an FFP model
#'
#' Binds one or more view specifications to an `ffp_model`.
#'
#' View expressions are evaluated against the model scenario variables and the
#' environment in which each view was created. Once a view is attached, the
#' resolved feature values are stored in the model, so subsequent changes to
#' objects in the calling environment do not alter the meaning of the model.
#'
#' A prior must be specified before views are attached. Views are cumulative:
#' adding a new view does not replace views already stored in the model.
#'
#' @param ... One or more `ffp_view_spec` objects created by functions such as
#'   [view_mean()], [view_probability()], [view_quantile()],
#'   [view_median()], [view_rank()], [view_volatility()],
#'   [view_variance()], [view_covariance()], or [view_correlation()].
#'
#' @return The updated `ffp_model`.
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
#'   recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(prior_uniform()) |>
#'   ffp_view(
#'     view_mean(x = equity, target = 0.01),
#'     view_probability(x = inflation > 0.04, target = 0.25)
#'   )
#' model
#'
#' # External features are matched by position
#' signal <- c(-0.2, -0.1, 0, 0.2, 0.3)
#'
#' model <- model |>
#'   ffp_view(
#'     view_mean(x = signal, target = 0.05)
#'   )
ffp_view <- function(model, ...) {

  call <- rlang::caller_env()

  validate_ffp_model(model = model, call = call)
  validate_model_prior_for_views(model = model, call = call)

  views <- unname(rlang::list2(...))
  validate_view_collection(views = views, call = call)

  for (view in views) {
    bound_view <- bind_view(view = view, model = model, call = call)
    model$views[[length(model$views) + 1L]] <- bound_view
  }

  model

}


# Bound-view constructors -------------------------------------------------

new_bound_view <- function(method, features, target, parameters = list(), metadata = list(), class) {

  structure(
    list(
      method     = method,
      features   = features,
      target     = target,
      parameters = parameters,
      metadata   = metadata
    ),
    class = c(class, "ffp_view")
  )
}


# View binding ------------------------------------------------------------

#' @noRd
bind_view <- function(view, model, call = rlang::caller_env()) {
  UseMethod("bind_view")
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_mean_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_mean_features(features = features, call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "mean",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_mean"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_probability_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_probability_features(features = features, arg = "x", call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  given            <- NULL
  given_expression <- NULL

  if (!is.null(view$parameters$given)) {

    given <- resolve_view_features(expression = view$parameters$given, model = model, arg = "given", call = call)

    validate_probability_features(features = given, arg = "given", call = call)
    validate_single_conditioning_event(features = given, call = call)
    validate_positive_conditioning_probability(features = given, model = model, call = call)

    given_expression <- compact_view_label(view$parameters$given)

  }

  new_bound_view(
    method = "probability",
    features = features,
    target = view$target,
    parameters = list(
      given = given
    ),
    metadata = list(expression = compact_view_label(view$x), given_expression = given_expression),
    class = "ffp_view_probability"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_quantile_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_quantile_features(features = features, call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "quantile",
    features = features,
    target = view$target,
    parameters = list(
      level = view$parameters$level
    ),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_quantile"
  )
}

#' @exportS3Method
#' @noRd
bind_view.ffp_view_median_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_median_features(features = features, call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "median",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_median"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_rank_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)
  indices  <- resolve_rank_order(order = view$parameters$order, features = features, call = call)
  features <- subset_rank_features(features = features, indices = indices)

  validate_rank_features(features = features, call = call)

  new_bound_view(
    method = "rank",
    features = features,
    target = NULL,
    parameters = list(),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_rank"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_volatility_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_volatility_features(features = features, call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "volatility",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_volatility"
  )
}



#' @exportS3Method
#' @noRd
bind_view.ffp_view_spec <- function(view, model, call = rlang::caller_env()) {

  ffp_abort(
    c(
      "Unsupported view specification.",
      "x" = paste0("No binding method is available for class `", class(view)[[1]], "`.")
    ),
    class = "ffp_error_invalid_view",
    call = call
  )
}


#' @exportS3Method
#' @noRd
bind_view.default <- function(view, model, call = rlang::caller_env()) {
  ffp_abort(
    c(
      "Unsupported view specification.",
      "x" = "{.arg view} must inherit from {.cls ffp_view_spec}."
    ),
    class = "ffp_error_invalid_view",
    call = call
  )
}


# Feature resolution ------------------------------------------------------

resolve_view_features <- function(expression, model, arg, call = rlang::caller_env()) {

  data  <- scenario_data_mask(model$scenarios)
  label <- compact_view_label(expression)

  values <- tryCatch(
    rlang::eval_tidy(expression, data = data),
    error = function(error) {
      ffp_abort(
        c(
          paste0("Failed to evaluate {.arg ", arg, "} for the view."),
          "x" = paste0("Could not evaluate: ", label),
          "i" = paste0(
            "View expressions are evaluated against the scenario variables ",
            "and the environment in which the view was created."
          )
        ),
        class = c(
          "ffp_error_invalid_view_expression",
          "ffp_error_invalid_view"
        ),
        parent = error,
        call = call
      )
    }
  )

  as_scenario_features(
    x = values,
    n_scenarios = scenario_count(model$scenarios),
    arg = arg,
    expression_label = label,
    call = call
  )
}


# Scenario-feature interface ---------------------------------------------

new_ffp_feature_set <- function(values, feature_names = NULL, source_class = NULL) {
  structure(
    list(values = values, names = feature_names, source_class = source_class),
    class = "ffp_feature_set"
  )
}


#' @noRd
as_scenario_features <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {
  UseMethod("as_scenario_features")
}


#' @exportS3Method
#' @noRd
as_scenario_features.matrix <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  features <- new_ffp_feature_set(values = x, feature_names = colnames(x), source_class = first_class(x))
  validate_feature_set(features = features, n_scenarios = n_scenarios, arg = arg, expression_label = expression_label, call = call)

  features
}


#' @exportS3Method
#' @noRd
as_scenario_features.array <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  if (length(dim(x)) > 2L) {
    ffp_abort(
      c(
        paste0("{.arg ", arg, "} must have at most two dimensions."),
        "x" = "Arrays with more than two dimensions are not supported.",
        "i" = "Rows must correspond to model scenarios."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  as_scenario_features.matrix(
    x = as.matrix(x),
    n_scenarios = n_scenarios,
    arg = arg,
    expression_label = expression_label,
    call = call
  )

}


#' @exportS3Method
#' @noRd
as_scenario_features.data.frame <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  list_columns <- vapply(x, is.list,logical(1))

  if (any(list_columns)) {
    column_names <- names(x)[list_columns]
    column_names <- paste0("`", column_names, "`", collapse = ", ")

    ffp_abort(
      c(
        paste0("{.arg ", arg, "} must not contain list-columns."),
        "x" = paste0("Found list-column(s): ", column_names, "."),
        "i" = "Each feature must be stored in an atomic column."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  features <- new_ffp_feature_set(values = x, feature_names = names(x), source_class = first_class(x))
  validate_feature_set(features = features, n_scenarios = n_scenarios, arg = arg, expression_label = expression_label, call = call)

  features
}


#' @exportS3Method
#' @noRd
as_scenario_features.xts <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  values   <- as.matrix(x)
  features <- new_ffp_feature_set(values = values, feature_names = colnames(values), source_class = first_class(x))

  validate_feature_set(features = features, n_scenarios = n_scenarios, arg = arg, expression_label = expression_label, call = call)

  features
}


#' @exportS3Method
#' @noRd
as_scenario_features.ts <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  values        <- if (is.null(dim(x))) as.vector(x) else as.matrix(x)
  feature_names <- if (is.null(dim(values))) NULL else colnames(values)

  features <- new_ffp_feature_set(values = values, feature_names = feature_names, source_class = first_class(x))

  validate_feature_set(features = features, n_scenarios = n_scenarios, arg = arg, expression_label = expression_label, call = call)

  features

}


#' @exportS3Method
#' @noRd
as_scenario_features.default <- function(x, n_scenarios, arg = "x", expression_label = NULL, call = rlang::caller_env()) {

  if (is.atomic(x) && is.null(dim(x))) {
    features <- new_ffp_feature_set(values = x, feature_names = NULL, source_class = first_class(x))
    validate_feature_set(features = features, n_scenarios = n_scenarios, arg = arg, expression_label = expression_label, call = call)
    return(features)
  }

  ffp_abort(
    c(
      paste0("Unsupported feature input in {.arg ", arg, "}."),
      "x" = paste0(
        "Received an object of class `",
        first_class(x),
        "`."
      ),
      "i" = paste0(
        "Use an atomic vector, matrix, data frame, tibble, {.cls ts}, ",
        "{.cls mts}, or {.cls xts} object with one row per scenario."
      )
    ),
    class = c(
      "ffp_error_invalid_view_features",
      "ffp_error_invalid_view"
    ),
    call = call
  )

}


feature_count <- function(x) {
  values <- x$values
  if (is.atomic(values) && is.null(dim(values))) return(1L)
  NCOL(values)
}

feature_observation_count <- function(x) NROW(x$values)

feature_column <- function(x, i = 1L) {

  values <- x$values

  if (is.atomic(values) && is.null(dim(values))) {
    return(unname(values))
  }

  if (inherits(values, "data.frame")) {
    return(unname(values[[i]]))
  }

  unname(values[, i])
}


# View validation ---------------------------------------------------------

validate_view_x_spec <- function(x, constructor, call = rlang::caller_env()) {

  if (rlang::quo_is_missing(x)) {
    ffp_abort(
      c(
        paste0("{.fn ", constructor, "} requires {.arg x}."),
        "i" = "Supply a scenario feature or expression defining the view."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  invisible(x)
}


validate_mean_target <- function(target, call = rlang::caller_env()) {
  validate_numeric_view_target(target = target, call = call)
  target
}

validate_quantile_target <- function(target, call = rlang::caller_env()) {
  validate_numeric_view_target(target = target, call = call)
  target
}

resolve_quantile_level <- function(level, n_targets, call = rlang::caller_env()) {

  if (!is.numeric(level) || !is.null(dim(level))) {
    ffp_abort(
      c(
        "{.arg level} must be a numeric vector.",
        "i" = "Supply one common level or exactly one level per target."
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (length(level) == 0L) {
    ffp_abort(
      "{.arg level} must contain at least one value.",
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(level)) {
    ffp_abort(
      c(
        "{.arg level} must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (any(!is.finite(level))) {
    ffp_abort(
      c(
        "{.arg level} must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (any(level <= 0 | level >= 1)) {
    ffp_abort(
      c(
        "{.arg level} must be strictly between 0 and 1.",
        "i" = "Values 0 and 1 correspond to support extremes, not interior quantiles."
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (!(length(level) %in% c(1L, n_targets))) {
    ffp_abort(
      c(
        "{.arg level} must contain one common level or one value per target.",
        "x" = paste0(
          "Received ",
          length(level),
          " level value(s) for ",
          n_targets,
          " target value(s)."
        ),
        "i" = paste0(
          "A scalar {.arg level} applies the same quantile level to every ",
          "target; otherwise the sizes must match exactly."
        )
      ),
      class = c(
        "ffp_error_invalid_view_level",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (length(level) == 1L) {
    level <- rep(level, n_targets)
  }

  unname(as.double(level))
}


validate_median_target <- function(target, call = rlang::caller_env()) {
  validate_numeric_view_target(target = target, call = call)
  target
}

validate_rank_order_spec <- function(order, call = rlang::caller_env()) {

  if (is.character(order)) {
    if (length(order) < 2L) {
      ffp_abort(
        c(
          "{.arg order} must identify at least two features.",
          "i" = "Supply at least two distinct feature names."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    if (anyNA(order) || any(!nzchar(order))) {
      ffp_abort(
        c(
          "Character {.arg order} values must be complete feature names.",
          "x" = "Missing or empty names are not supported."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    if (anyDuplicated(order)) {
      ffp_abort(
        c(
          "{.arg order} must not contain duplicated features.",
          "i" = "Each feature may appear only once in a ranking."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    return(unname(order))
  }

  if (is.numeric(order) && is.null(dim(order))) {
    if (length(order) < 2L) {
      ffp_abort(
        c(
          "{.arg order} must identify at least two features.",
          "i" = "Supply at least two distinct feature positions."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    if (anyNA(order) || any(!is.finite(order))) {
      ffp_abort(
        "Numeric {.arg order} values must be finite and non-missing.",
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    if (any(order < 1 | order != floor(order))) {
      ffp_abort(
        c(
          "Numeric {.arg order} values must be positive whole numbers.",
          "i" = "Feature positions use one-based indexing."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    if (anyDuplicated(order)) {
      ffp_abort(
        c(
          "{.arg order} must not contain duplicated features.",
          "i" = "Each feature may appear only once in a ranking."
        ),
        class = c(
          "ffp_error_invalid_view_order",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }

    return(as.integer(order))
  }

  ffp_abort(
    c(
      "{.arg order} must be a character vector or a numeric vector.",
      "i" = "Use feature names or one-based feature positions."
    ),
    class = c(
      "ffp_error_invalid_view_order",
      "ffp_error_invalid_view"
    ),
    call = call
  )
}


validate_probability_target <- function(target, call = rlang::caller_env()) {
  validate_numeric_view_target(target = target, call = call)
  if (any(target < 0 | target > 1)) {
    ffp_abort(
      c(
        "{.arg target} must contain probabilities between 0 and 1.",
        "i" = "Use decimal probabilities, such as 0.25 for 25 percent."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }
  target
}


validate_numeric_view_target <- function(target, call = rlang::caller_env()) {

  if (!is.numeric(target) || !is.null(dim(target))) {
    ffp_abort(
      c(
        "{.arg target} must be a numeric vector.",
        "x" = "Matrices, arrays, and non-numeric objects are not supported."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (length(target) == 0L) {
    ffp_abort(
      "{.arg target} must contain at least one value.",
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(target)) {
    ffp_abort(
      c(
        "{.arg target} must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (any(!is.finite(target))) {
    ffp_abort(
      c(
        "{.arg target} must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(target)
}


validate_view_collection <- function(views, call = rlang::caller_env()) {

  if (length(views) == 0L) {
    ffp_abort(
      c(
        "{.fn ffp_view} requires at least one view specification.",
        "i" = paste0(
          "Create views with constructors such as {.fn view_mean}, ",
          "{.fn view_probability}, {.fn view_quantile}, {.fn view_median}, ",
          "{.fn view_rank}, {.fn view_volatility}, {.fn view_variance}, ",
          "{.fn view_covariance}, or {.fn view_correlation}."
        )
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  valid <- vapply(views, inherits, logical(1), what = "ffp_view_spec")

  if (!all(valid)) {
    invalid <- which(!valid)[[1]]
    ffp_abort(
      c(
        "All inputs to {.fn ffp_view} must be view specifications.",
        "x" = paste0("Input ", invalid, " has class `", first_class(views[[invalid]]), "`."),
        "i" = paste0(
          "Create views with constructors such as {.fn view_mean}, ",
          "{.fn view_probability}, {.fn view_quantile}, {.fn view_median}, ",
          "{.fn view_rank}, or {.fn view_volatility}."
        )
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  invisible(views)
}


validate_model_prior_for_views <- function(model, call = rlang::caller_env()) {

  if (is.null(model$prior)) {
    ffp_abort(
      c(
        "An FFP model must have a prior before views can be attached.",
        "i" = "Specify the prior with {.fn ffp_prior} before calling {.fn ffp_view}."
      ),
      class = c(
        "ffp_error_missing_prior",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }
  invisible(model)
}


validate_feature_set <- function(features, n_scenarios, arg, expression_label = NULL, call = rlang::caller_env()) {

  n_observations <- feature_observation_count(features)
  n_features     <- feature_count(features)

  if (n_observations != n_scenarios) {
    detail <- if (is.null(expression_label)) {
      paste0("{.arg ", arg, "}")
    } else {
      paste0("`", expression_label, "`")
    }

    ffp_abort(
      c(
        paste0("{.arg ", arg, "} is incompatible with the scenario support."),
        "x" = paste0(
          detail,
          " contains ",
          n_observations,
          " observation(s), but the model contains ",
          n_scenarios,
          " scenario(s)."
        ),
        "i" = paste0(
          "External features are matched to model scenarios strictly by ",
          "position."
        )
      ),
      class = c(
        "ffp_error_incompatible_view",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (n_features == 0L) {
    ffp_abort(
      c(
        paste0("{.arg ", arg, "} must contain at least one feature."),
        "i" = "Each feature defines one quantity or event for the view."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  validate_feature_names(feature_names = features$names, arg = arg, call = call)

  invisible(features)
}


validate_feature_names <- function(feature_names, arg, call = rlang::caller_env()) {

  if (is.null(feature_names)) {
    return(invisible(feature_names))
  }

  unnamed <- is.na(feature_names) | !nzchar(feature_names)

  if (all(unnamed)) {
    return(invisible(feature_names))
  }

  if (any(unnamed)) {
    ffp_abort(
      c(
        paste0("Feature names in {.arg ", arg, "} must be complete."),
        "x" = "Some features are named while others are unnamed.",
        "i" = "Either name every feature or leave all features unnamed."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  duplicated_names <- unique(feature_names[duplicated(feature_names)])

  if (length(duplicated_names) > 0L) {
    duplicated_names <- paste0("`", duplicated_names, "`", collapse = ", ")

    ffp_abort(
      c(
        paste0("Feature names in {.arg ", arg, "} must be unique."),
        "x" = paste0("Duplicated name(s): ", duplicated_names, ".")
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(feature_names)
}


validate_mean_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_mean} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = "Use a quantitative scenario feature for a posterior mean view."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Mean-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Mean-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(features)
}


validate_quantile_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_quantile} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = "Use a quantitative scenario feature for a posterior quantile view."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Quantile-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Quantile-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(features)
}


validate_median_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_median} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = "Use a quantitative scenario feature for a posterior median view."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Median-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Median-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(features)
}


validate_rank_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_rank} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = "Ranking views compare posterior means of quantitative features."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Rank-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Rank-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (feature_count(features) < 2L) {
    ffp_abort(
      c(
        "{.fn view_rank} requires at least two features.",
        "i" = "Supply a panel containing two or more quantities to rank."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(features)
}


resolve_rank_order <- function(order, features, call = rlang::caller_env()) {

  n_features <- feature_count(features)

  if (is.character(order)) {
    feature_names <- features$names
    unnamed <- is.null(feature_names) || all(is.na(feature_names) | !nzchar(feature_names))

    if (unnamed) {
      ffp_abort(
        c(
          "Character {.arg order} requires named features.",
          "i" = "Name the features or select them by numeric position."
        ),
        class = c("ffp_error_invalid_view_order", "ffp_error_invalid_view"),
        call = call
      )
    }

    indices <- match(order, feature_names)

    if (anyNA(indices)) {
      unknown <- unique(order[is.na(indices)])
      unknown <- paste0("`", unknown, "`", collapse = ", ")

      ffp_abort(
        c(
          "Unknown feature name in {.arg order}.",
          "x" = paste0("Unknown name(s): ", unknown, "."),
          "i" = "Use names present in the resolved scenario features."
        ),
        class = c("ffp_error_invalid_view_order", "ffp_error_invalid_view"),
        call = call
      )
    }

    return(indices)
  }

  if (any(order > n_features)) {
    ffp_abort(
      c(
        "A position in {.arg order} exceeds the number of available features.",
        "x" = paste0(
          "The largest requested position is ",
          max(order),
          ", but {.arg x} contains ",
          n_features,
          " feature(s)."
        )
      ),
      class = c(
        "ffp_error_invalid_view_order",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  order
}


subset_rank_features <- function(features, indices) {

  values <- features$values

  selected_values <- if (inherits(values, "data.frame")) {
    values[ , indices, drop = FALSE]
  } else {
    values[ , indices, drop = FALSE]
  }

  selected_names <- if (is.null(features$names)) {
    NULL
  } else {
    features$names[indices]
  }

  new_ffp_feature_set(values = selected_values, feature_names = selected_names, source_class = features$source_class)

}


validate_probability_features <- function(features, arg, call = rlang::caller_env()) {

  values <- features$values

  logical_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.logical, logical(1))
  } else {
    is.logical(values)
  }

  if (!all(logical_features)) {
    ffp_abort(
      c(
        paste0("{.arg ", arg, "} must define logical event features."),
        "x" = paste0(
          "At least one feature in {.arg ",
          arg,
          "} is not logical."
        ),
        "i" = paste0(
          "Use logical expressions such as `x > 0`, `regime == \"crisis\"`, ",
          "or combinations with `&` and `|`."
        )
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        paste0("{.arg ", arg, "} must not contain missing event values."),
        "x" = "Found at least one {.val NA} result.",
        "i" = "Every scenario must resolve unambiguously to TRUE or FALSE."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(features)
}


validate_target_feature_compatibility <- function(target, features, call = rlang::caller_env()) {

  n_targets  <- length(target)
  n_features <- feature_count(features)

  if (n_targets != n_features) {
    ffp_abort(
      c(
        "{.arg target} must contain one value for each feature in {.arg x}.",
        "x" = paste0(
          "{.arg x} contains ",
          n_features,
          " feature(s), but {.arg target} contains ",
          n_targets,
          " value(s)."
        ),
        "i" = "Targets are matched to features by position and are never recycled."
      ),
      class = c("ffp_error_incompatible_view", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(target)
}


validate_single_conditioning_event <- function(features, call = rlang::caller_env()) {

  n_features <- feature_count(features)

  if (n_features != 1L) {
    ffp_abort(
      c(
        "{.arg given} must define exactly one conditioning event.",
        "x" = paste0(
          "{.arg given} currently defines ",
          n_features,
          " event(s)."
        ),
        "i" = paste0(
          "Use separate {.fn view_probability} specifications when different ",
          "events require different conditioning information."
        )
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(features)
}


validate_positive_conditioning_probability <- function(features, model, call = rlang::caller_env()) {

  conditioning_event <- feature_column(features, i = 1L)
  probability <- sum(model$prior[conditioning_event])

  if (!is.finite(probability) || probability <= 0) {
    ffp_abort(
      c(
        "The conditioning event in {.arg given} has zero prior probability.",
        "x" = "A conditional probability is undefined when `P(given) = 0`.",
        "i" = paste0(
          "Choose a conditioning event with positive support under the model ",
          "prior or revise the prior specification."
        )
      ),
      class = c(
        "ffp_error_zero_conditioning_probability",
        "ffp_error_incompatible_view"
      ),
      call = call
    )
  }

  invisible(probability)
}

validate_volatility_target <- function(target, call = rlang::caller_env()) {

  validate_numeric_view_target(target = target, call = call)

  if (any(target < 0)) {
    ffp_abort(
      c(
        "{.arg target} must contain non-negative volatility values.",
        "i" = "Use zero or a positive value for each volatility target."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  target
}


validate_volatility_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_volatility} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = paste0(
          "Use a quantitative scenario feature for a posterior volatility view."
        )
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Volatility-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Volatility-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(features)
}


has_non_finite_feature_values <- function(values) {

  if (inherits(values, "data.frame")) {
    return(
      any(
        vapply(
          values,
          function(column) any(!is.finite(column)),
          logical(1)
        )
      )
    )
  }

  any(!is.finite(values))
}


first_class <- function(x) {

  input_class <- class(x)

  if (length(input_class) == 0L) {
    return(typeof(x))
  }

  input_class[[1]]
}


view_constraint_count <- function(x) {
  if (x$method %in% c("quantile", "median")) {
    return(2L * length(x$target))
  }

  if (identical(x$method, "rank")) {
    return(feature_count(x$features) - 1L)
  }

  if (x$method %in% c("covariance", "correlation")) {
    k <- feature_count(x$features)
    return(as.integer(k * (k + 1L) / 2L))
  }

  length(x$target)
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_view_spec <- function(x, ...) {
  cat("<ffp_view_spec>\n")

  cat(
    "Method:      ",
    format_view_method(x$method),
    "\n",
    sep = ""
  )

  if (!identical(x$method, "rank")) {
    cat(
      "Targets:     ",
      length(x$target),
      "\n",
      sep = ""
    )
  }

  cat(
    "Expression:  ",
    compact_view_label(x$x),
    "\n",
    sep = ""
  )

  if (identical(x$method, "probability")) {
    conditional <- !is.null(x$parameters$given)

    cat(
      "Conditional: ",
      if (conditional) "yes" else "no",
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "quantile")) {
    cat(
      "Level:       ",
      format_view_level(x$parameters$level),
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "rank")) {
    cat(
      "Order:       ",
      format_rank_spec_order(x$parameters$order),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}


#' @export
#' @noRd
print.ffp_view <- function(x, ...) {
  cat(
    "<ffp_view_",
    x$method,
    ">\n",
    sep = ""
  )

  cat(
    "Features:    ",
    feature_count(x$features),
    "\n",
    sep = ""
  )

  cat(
    "Constraints: ",
    view_constraint_count(x),
    "\n",
    sep = ""
  )

  cat(
    "Expression:  ",
    x$metadata$expression,
    "\n",
    sep = ""
  )

  if (identical(x$method, "probability")) {
    conditional <- !is.null(x$parameters$given)

    cat(
      "Conditional: ",
      if (conditional) "yes" else "no",
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "quantile")) {
    cat(
      "Level:       ",
      format_view_level(x$parameters$level),
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "rank")) {
    cat(
      "Order:       ",
      format_bound_rank_order(x$features),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}


format_view_method <- function(method) {
  switch(
    method,
    mean = "mean",
    probability = "probability",
    quantile = "quantile",
    median = "median",
    rank = "rank",
    volatility = "volatility",
    variance = "variance",
    covariance = "covariance",
    correlation = "correlation",
    method
  )
}


format_view_level <- function(level) {
  if (length(level) == 0L) {
    return("not specified")
  }

  if (all(level == level[[1]])) {
    return(
      format(level[[1]], digits = 15, scientific = FALSE, trim = TRUE)
    )
  }
  "mixed"
}


format_rank_spec_order <- function(order) {
  compact_rank_order(order)
}


format_bound_rank_order <- function(features) {
  feature_names <- features$names
  use_names <- !is.null(feature_names) &&
    all(!is.na(feature_names) & nzchar(feature_names))

  labels <- if (use_names) {
    feature_names
  } else {
    seq_len(feature_count(features))
  }

  compact_rank_order(labels)
}


compact_rank_order <- function(x, width = 60L) {
  label <- paste(x, collapse = " >= ")

  if (nchar(label, type = "width") <= width) {
    return(label)
  }

  paste0(
    substr(label, 1L, width - 3L),
    "..."
  )
}


compact_view_label <- function(x, width = 60L) {
  label <- rlang::as_label(x)
  label <- gsub("[[:space:]]+", " ", label)
  if (nchar(label, type = "width") <= width) {
    return(label)
  }
  paste0(substr(label, 1L, width - 3L), "...")
}

# Correlation view --------------------------------------------------------

#' Correlation view
#'
#' Creates an equality view on a posterior correlation matrix using the linear
#' Fully Flexible Views formulation of Meucci (2008).
#'
#' `x` must resolve to a numeric scenario panel with at least two features.
#' `target` specifies the complete correlation matrix associated with those
#' features.
#'
#' External features are matched to model scenarios strictly by position. The
#' package does not reorder, join, recycle, or otherwise align observations
#' automatically.
#'
#' @section Mathematical definition:
#'
#' Let `x` resolve to `K` scenario features and let \eqn{C^{target}} be the
#' `K` by `K` correlation matrix supplied in `target`.
#'
#' For each pair \eqn{1 \le k \le l \le K}, the Fully Flexible Views
#' formulation represents the correlation view through
#'
#' \deqn{
#'   \sum_{t=1}^{T} \tilde p_t X_{t,k} X_{t,l}
#'   =
#'   \hat m_k \hat m_l +
#'   \hat \sigma_k \hat \sigma_l C^{target}_{k,l},
#' }
#'
#' where \eqn{\hat m_k} and \eqn{\hat \sigma_k} are the reference mean and
#' standard deviation of feature `k` under the model prior.
#'
#' The reference moments are internal quantities used when the view is later
#' compiled for Entropy Pooling. They are not additional user parameters.
#'
#' Because the correlation matrix is symmetric, only one triangular portion
#' needs to be compiled. For `K` features, one correlation view therefore
#' represents `K * (K + 1) / 2` mathematical constraints.
#'
#' `target` must be a valid correlation matrix: numeric, square, symmetric,
#' finite, positive semidefinite, with unit diagonal and entries between
#' -1 and 1.
#'
#' Target rows and columns correspond to the features in `x` by position.
#' When both the features and `target` are named, their names must already
#' appear in the same order; the package never silently reorders them.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   panel or expression resolving to at least two features, with one row per
#'   scenario.
#' @param target A finite numeric correlation matrix with one row and column
#'   for every feature in `x`.
#'
#' @return An object of class `ffp_view_correlation_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_volatility()]
#'
#' @export
#'
#' @examples
#' target_cor <- matrix(
#'   c(
#'     1.00, 0.70, 0.40,
#'     0.70, 1.00, 0.55,
#'     0.40, 0.55, 1.00
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#'
#' view_correlation(
#'   x = returns,
#'   target = target_cor
#' )
view_correlation <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_correlation} requires {.arg target}.",
        "i" = "Supply the complete target correlation matrix."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_correlation", call = call)
  target <- validate_correlation_target(target = target, call = call)

  new_view_spec(
    method = "correlation",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_correlation_spec"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_correlation_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_correlation_features(features = features,call = call)
  validate_correlation_compatibility(target = view$target, features = features, call = call)
  validate_correlation_reference_dispersion(features = features, model = model, call = call)

  new_bound_view(
    method = "correlation",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(
      expression = compact_view_label(view$x)
    ),
    class = "ffp_view_correlation"
  )
}


validate_correlation_target <- function(target, call = rlang::caller_env()) {

  tolerance <- sqrt(.Machine$double.eps)

  if (!is.matrix(target) || !is.numeric(target)) {
    ffp_abort(
      c(
        "{.arg target} must be a numeric matrix.",
        "i" = "Supply the complete target correlation matrix."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (nrow(target) != ncol(target) || nrow(target) < 2L) {
    ffp_abort(
      c(
        "{.arg target} must be a square matrix with at least two features.",
        "x" = paste0(
          "Received a ",
          nrow(target),
          " by ",
          ncol(target),
          " matrix."
        )
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (anyNA(target)) {
    ffp_abort(
      c(
        "{.arg target} must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (any(!is.finite(target))) {
    ffp_abort(
      c(
        "{.arg target} must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (any(target < -1 - tolerance | target > 1 + tolerance)) {
    ffp_abort(
      c(
        "Entries in {.arg target} must be between -1 and 1.",
        "i" = "Supply a valid correlation matrix."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (max(abs(target - t(target))) > tolerance) {
    ffp_abort(
      c(
        "{.arg target} must be symmetric.",
        "i" = "A correlation matrix must equal its transpose."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (any(abs(diag(target) - 1) > tolerance)) {
    ffp_abort(
      c(
        "{.arg target} must have ones on its diagonal.",
        "i" = "Each feature must have correlation one with itself."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  eigenvalues <- eigen(target, symmetric = TRUE, only.values = TRUE)$values

  if (min(eigenvalues) < -tolerance) {
    ffp_abort(
      c(
        "{.arg target} must be positive semidefinite.",
        "i" = "Supply a valid correlation matrix."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  validate_correlation_target_names(target = target, call = call)
  target

}


validate_correlation_target_names <- function(target, call = rlang::caller_env()) {

  row_names <- rownames(target)
  col_names <- colnames(target)

  if (is.null(row_names) && is.null(col_names)) {
    return(invisible(target))
  }

  if (is.null(row_names) || is.null(col_names)) {
    ffp_abort(
      c(
        "Dimnames in {.arg target} must be complete.",
        "x" = "Supply both row names and column names, or neither."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  invalid_names <- is.na(row_names) | !nzchar(row_names) | is.na(col_names) | !nzchar(col_names)

  if (any(invalid_names)) {
    ffp_abort(
      "{.arg target} must not contain missing or empty dimension names.",
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (anyDuplicated(row_names) || anyDuplicated(col_names)) {
    ffp_abort(
      "{.arg target} dimension names must be unique.",
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (!identical(row_names, col_names)) {
    ffp_abort(
      c(
        "Row and column names in {.arg target} must be identical.",
        "i" = "Use the same feature order on both dimensions."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(target)
}


validate_correlation_features <- function(features, call = rlang::caller_env()) {

  if (feature_count(features) < 2L) {
    ffp_abort(
      c(
        "{.fn view_correlation} requires at least two features.",
        "i" = "Supply a scenario panel containing the variables to correlate."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_correlation} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Correlation-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Correlation-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c("ffp_error_invalid_view_features", "ffp_error_invalid_view"),
      call = call
    )
  }

  invisible(features)
}


validate_correlation_compatibility <- function(target, features, call = rlang::caller_env()) {

  n_features <- feature_count(features)

  if (nrow(target) != n_features) {
    ffp_abort(
      c(
        "{.arg target} must match the features in {.arg x}.",
        "x" = paste0(
          "{.arg x} contains ",
          n_features,
          " feature(s), but {.arg target} is a ",
          nrow(target),
          " by ",
          ncol(target),
          " matrix."
        )
      ),
      class = c("ffp_error_incompatible_view", "ffp_error_invalid_view"),
      call = call
    )
  }

  target_names <- rownames(target)
  feature_names <- features$names

  if (!is.null(target_names) && !is.null(feature_names)) {
    if (!identical(target_names, feature_names)) {
      ffp_abort(
        c(
          "Names in {.arg target} must match the feature order in {.arg x}.",
          "i" = "The package never reorders correlation targets automatically."
        ),
        class = c(
          "ffp_error_incompatible_view",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }
  }

  invisible(target)
}


validate_correlation_reference_dispersion <- function(features, model, call = rlang::caller_env()) {

  for (i in seq_len(feature_count(features))) {

    values             <- feature_column(features, i = i)
    reference_mean     <- sum(model$prior * values)
    reference_variance <- sum(model$prior * (values - reference_mean)^2)

    if (!is.finite(reference_variance) || reference_variance <= 0) {
      feature <- if (is.null(features$names)) {
        paste0("feature ", i)
      } else {
        paste0("`", features$names[[i]], "`")
      }

      ffp_abort(
        c(
          "Correlation requires positive reference dispersion.",
          "x" = paste0(
            feature,
            " has zero variance under the model prior."
          )
        ),
        class = c("ffp_error_undefined_correlation", "ffp_error_incompatible_view"),
        call = call
      )
    }
  }

  invisible(features)
}


# Variance view -----------------------------------------------------------

#' Variance view
#'
#' Creates an equality view on posterior variance using the linear Fully
#' Flexible Views formulation underlying [view_volatility()].
#'
#' `x` can be a scenario variable, an expression evaluated from the scenario
#' data, or an external numeric vector or panel. External features are matched
#' to model scenarios strictly by position.
#'
#' If `x` resolves to multiple features, `target` must contain exactly one
#' variance target per feature. Targets are matched to features by position
#' and are never recycled automatically.
#'
#' @section Mathematical definition:
#'
#' For a scenario feature \eqn{X_k}, variance target \eqn{v_k}, and posterior
#' probabilities \eqn{\tilde p}, the view is represented by
#'
#' \deqn{
#'   \sum_{t=1}^{T} \tilde p_t X_{t,k}^2
#'   =
#'   \hat m_k^2 + v_k,
#' }
#'
#' where \eqn{\hat m_k} is the reference mean of the feature.
#'
#' This is the variance-unit parameterization of the same linear second-moment
#' formulation used by [view_volatility()]. Accordingly, `target` is expressed
#' in squared units rather than standard-deviation units.
#'
#' Variance targets must be non-negative. Zero is allowed.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   feature or expression. It may resolve to a vector or a rectangular panel
#'   with one row per scenario.
#' @param target A finite, non-negative numeric vector containing exactly one
#'   variance target per feature in `x`.
#'
#' @return An object of class `ffp_view_variance_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_volatility()], [view_correlation()]
#'
#' @export
#'
#' @examples
#' view_variance(
#'   x = equity,
#'   target = 0.0225
#' )
#'
#' view_variance(
#'   x = returns,
#'   target = c(0.0225, 0.0100)
#' )
view_variance <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_variance} requires {.arg target}.",
        "i" = "Supply exactly one variance target per feature in {.arg x}."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_variance", call = call)
  target <- validate_variance_target(target = target, call = call)

  new_view_spec(
    method = "variance",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_variance_spec"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_variance_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_variance_features(features = features, call = call)
  validate_target_feature_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "variance",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(expression = compact_view_label(view$x)),
    class = "ffp_view_variance"
  )
}


validate_variance_target <- function(target, call = rlang::caller_env()) {

  validate_numeric_view_target(target = target,call = call)

  if (any(target < 0)) {
    ffp_abort(
      c(
        "{.arg target} must contain non-negative variance values.",
        "i" = "Use zero or a positive value for each variance target."
      ),
      class = c("ffp_error_invalid_view_target", "ffp_error_invalid_view"),
      call = call
    )
  }

  target
}


validate_variance_features <- function(features, call = rlang::caller_env()) {

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_variance} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric.",
        "i" = "Use a quantitative scenario feature for a variance view."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Variance-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Variance-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(features)
}

# Covariance view ---------------------------------------------------------

#' Covariance view
#'
#' Creates an equality view on a posterior covariance matrix using the linear
#' Fully Flexible Views formulation of Meucci (2008).
#'
#' `x` must resolve to a numeric scenario panel with at least two features.
#' `target` specifies the complete covariance matrix associated with those
#' features.
#'
#' External features are matched to model scenarios strictly by position. The
#' package does not reorder, join, recycle, or otherwise align observations
#' automatically.
#'
#' @section Mathematical definition:
#'
#' Let `x` resolve to `K` scenario features and let \eqn{C^{target}} be the
#' `K` by `K` covariance matrix supplied in `target`.
#'
#' For each pair \eqn{1 \le k \le l \le K}, the Fully Flexible Views
#' formulation represents the covariance view through
#'
#' \deqn{
#'   \sum_{t=1}^{T} \tilde p_t X_{t,k} X_{t,l}
#'   =
#'   \hat m_k \hat m_l + C^{target}_{k,l},
#' }
#'
#' where \eqn{\hat m_k} is the reference mean of feature `k` under the model
#' prior.
#'
#' The reference means are internal quantities used when the view is later
#' compiled for Entropy Pooling. They are not additional user parameters.
#'
#' Because the covariance matrix is symmetric, only one triangular portion
#' needs to be compiled. For `K` features, one covariance view therefore
#' represents `K * (K + 1) / 2` mathematical constraints.
#'
#' `target` must be a valid covariance matrix: numeric, square, symmetric,
#' finite, and positive semidefinite.
#'
#' Target rows and columns correspond to the features in `x` by position.
#' When both the features and `target` are named, their names must already
#' appear in the same order; the package never silently reorders them.
#'
#' @param x <[`data-masking`][rlang::args_data_masking]> A numeric scenario
#'   panel or expression resolving to at least two features, with one row per
#'   scenario.
#' @param target A finite numeric covariance matrix with one row and column
#'   for every feature in `x`.
#'
#' @return An object of class `ffp_view_covariance_spec` and `ffp_view_spec`.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
#'
#' @seealso [ffp_view()], [view_volatility()], [view_correlation()]
#'
#' @export
#'
#' @examples
#' target_cov <- matrix(
#'   c(
#'     0.0400, 0.0180, 0.0120,
#'     0.0180, 0.0225, 0.0090,
#'     0.0120, 0.0090, 0.0100
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#'
#' view_covariance(
#'   x = returns,
#'   target = target_cov
#' )
view_covariance <- function(x, target) {

  call <- rlang::caller_env()

  if (missing(target)) {
    ffp_abort(
      c(
        "{.fn view_covariance} requires {.arg target}.",
        "i" = "Supply the complete target covariance matrix."
      ),
      class = "ffp_error_invalid_view",
      call = call
    )
  }

  x <- rlang::enquo(x)
  validate_view_x_spec(x = x, constructor = "view_covariance", call = call)
  target <- validate_covariance_target(target = target, call = call)

  new_view_spec(
    method = "covariance",
    x = x,
    target = target,
    parameters = list(),
    class = "ffp_view_covariance_spec"
  )
}


#' @exportS3Method
#' @noRd
bind_view.ffp_view_covariance_spec <- function(view, model, call = rlang::caller_env()) {

  features <- resolve_view_features(expression = view$x, model = model, arg = "x", call = call)

  validate_covariance_features(features = features, call = call)
  validate_covariance_compatibility(target = view$target, features = features, call = call)

  new_bound_view(
    method = "covariance",
    features = features,
    target = view$target,
    parameters = list(),
    metadata = list(
      expression = compact_view_label(view$x)
    ),
    class = "ffp_view_covariance"
  )
}


validate_covariance_target <- function(target, call = rlang::caller_env()) {

  tolerance <- sqrt(.Machine$double.eps)

  if (!is.matrix(target) || !is.numeric(target)) {
    ffp_abort(
      c(
        "{.arg target} must be a numeric matrix.",
        "i" = "Supply the complete target covariance matrix."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (nrow(target) != ncol(target) || nrow(target) < 2L) {
    ffp_abort(
      c(
        "{.arg target} must be a square matrix with at least two features.",
        "x" = paste0(
          "Received a ",
          nrow(target),
          " by ",
          ncol(target),
          " matrix."
        )
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(target)) {
    ffp_abort(
      c(
        "{.arg target} must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (any(!is.finite(target))) {
    ffp_abort(
      c(
        "{.arg target} must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (max(abs(target - t(target))) > tolerance) {
    ffp_abort(
      c(
        "{.arg target} must be symmetric.",
        "i" = "A covariance matrix must equal its transpose."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  eigenvalues <- eigen(target, symmetric = TRUE, only.values = TRUE)$values

  if (min(eigenvalues) < -tolerance) {
    ffp_abort(
      c(
        "{.arg target} must be positive semidefinite.",
        "i" = "Supply a valid covariance matrix."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  validate_covariance_target_names(target = target, call = call)

  target
}


validate_covariance_target_names <- function(target, call = rlang::caller_env()) {

  row_names <- rownames(target)
  col_names <- colnames(target)

  if (is.null(row_names) && is.null(col_names)) {
    return(invisible(target))
  }

  if (is.null(row_names) || is.null(col_names)) {
    ffp_abort(
      c(
        "Dimnames in {.arg target} must be complete.",
        "x" = "Supply both row names and column names, or neither."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invalid_names <- is.na(row_names) | !nzchar(row_names) | is.na(col_names) | !nzchar(col_names)

  if (any(invalid_names)) {
    ffp_abort(
      "{.arg target} must not contain missing or empty dimension names.",
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyDuplicated(row_names) || anyDuplicated(col_names)) {
    ffp_abort(
      "{.arg target} dimension names must be unique.",
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (!identical(row_names, col_names)) {
    ffp_abort(
      c(
        "Row and column names in {.arg target} must be identical.",
        "i" = "Use the same feature order on both dimensions."
      ),
      class = c(
        "ffp_error_invalid_view_target",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(target)
}


validate_covariance_features <- function(features, call = rlang::caller_env()) {

  if (feature_count(features) < 2L) {
    ffp_abort(
      c(
        "{.fn view_covariance} requires at least two features.",
        "i" = "Supply a scenario panel containing the variables to covary."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  values <- features$values

  numeric_features <- if (inherits(values, "data.frame")) {
    vapply(values, is.numeric, logical(1))
  } else {
    is.numeric(values)
  }

  if (!all(numeric_features)) {
    ffp_abort(
      c(
        "{.fn view_covariance} requires numeric features.",
        "x" = "At least one feature in {.arg x} is not numeric."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "Covariance-view features must not contain missing values.",
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  if (has_non_finite_feature_values(values)) {
    ffp_abort(
      c(
        "Covariance-view features must contain only finite values.",
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = c(
        "ffp_error_invalid_view_features",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  invisible(features)
}


validate_covariance_compatibility <- function(target, features, call = rlang::caller_env()) {

  n_features <- feature_count(features)

  if (nrow(target) != n_features) {
    ffp_abort(
      c(
        "{.arg target} must match the features in {.arg x}.",
        "x" = paste0(
          "{.arg x} contains ",
          n_features,
          " feature(s), but {.arg target} is a ",
          nrow(target),
          " by ",
          ncol(target),
          " matrix."
        )
      ),
      class = c(
        "ffp_error_incompatible_view",
        "ffp_error_invalid_view"
      ),
      call = call
    )
  }

  target_names <- rownames(target)
  feature_names <- features$names

  if (!is.null(target_names) && !is.null(feature_names)) {
    if (!identical(target_names, feature_names)) {
      ffp_abort(
        c(
          "Names in {.arg target} must match the feature order in {.arg x}.",
          "i" = "The package never reorders covariance targets automatically."
        ),
        class = c(
          "ffp_error_incompatible_view",
          "ffp_error_invalid_view"
        ),
        call = call
      )
    }
  }

  invisible(target)
}



