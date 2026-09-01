# Prior specifications ----------------------------------------------------

new_prior_spec <- function(method, parameters = list(), class) {
  structure(
    list(
      method = method,
      parameters = parameters
    ),
    class = c(class, "ffp_prior_spec")
  )
}


#' Uniform prior
#'
#' Creates a prior specification that assigns equal probability to every
#' scenario in an FFP model.
#'
#' The probabilities are realized only when the specification is applied to
#' an `ffp_model` with [ffp_prior()].
#'
#' @return An object of class `ffp_prior_uniform` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' prior_uniform()
#'
#' model <- ffp_model(c(-0.02, 0.01, 0.03))
#' model <- ffp_prior(model, prior_uniform())
prior_uniform <- function() {
  new_prior_spec(
    method = "uniform",
    parameters = list(),
    class = "ffp_prior_uniform"
  )
}


#' Custom prior
#'
#' Creates a prior specification from a user-supplied vector of
#' probabilities.
#'
#' Probabilities are matched to scenarios strictly by position. Names attached
#' to `probabilities` are ignored and removed with a warning.
#'
#' @param probabilities A numeric vector of non-negative probabilities that
#'   sums to one.
#'
#' @return An object of class `ffp_prior_custom` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' prior_custom(c(0.2, 0.3, 0.5))
prior_custom <- function(probabilities) {
  call <- rlang::caller_env()

  validate_probability_vector(
    probabilities = probabilities,
    arg = "probabilities",
    call = call
  )

  if (!is.null(names(probabilities))) {
    cli::cli_warn(
      c(
        "Names in {.arg probabilities} are ignored.",
        "i" = "Probabilities are matched to scenarios by position.",
        "i" = "Scenario labels and indexes belong to the scenario data."
      ),
      class = c(
        "ffp_warning_named_probabilities",
        "ffp_warning"
      ),
      call = call
    )
  }

  probabilities <- canonicalize_probabilities(probabilities)

  new_prior_spec(
    method = "custom",
    parameters = list(
      probabilities = probabilities
    ),
    class = "ffp_prior_custom"
  )
}


#' Exponential-decay prior
#'
#' Creates a prior specification that assigns progressively greater
#' probability to later scenarios using exponential decay.
#'
#' `half_life` is measured in observations. A scenario that is `half_life`
#' observations older than another receives half of its unnormalized weight.
#'
#' The last scenario is treated as the most recent observation. If the model
#' contains a temporal scenario index, the index must therefore be ordered
#' from oldest to newest. The scenario data are never reordered automatically.
#'
#' @param half_life A positive finite number giving the half-life in
#'   observations.
#'
#' @return An object of class `ffp_prior_exp_decay` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' prior_exp_decay(half_life = 42)
#'
#' model <- ffp_model(c(-0.02, 0.01, 0.03))
#' model <- ffp_prior(
#'   model,
#'   prior_exp_decay(half_life = 2)
#' )
prior_exp_decay <- function(half_life) {
  call <- rlang::caller_env()

  validate_half_life(
    half_life = half_life,
    call = call
  )

  new_prior_spec(
    method = "exp_decay",
    parameters = list(
      half_life = as.double(half_life)
    ),
    class = "ffp_prior_exp_decay"
  )
}


#' Rolling-window prior
#'
#' Creates a prior specification that assigns equal probability to the most
#' recent scenarios inside a rolling window and zero probability to all
#' earlier scenarios.
#'
#' `window` is measured in observations. The last scenario is treated as the
#' most recent observation. If the model contains a temporal scenario index,
#' the index must therefore be ordered from oldest to newest. The scenario
#' data are never reordered automatically.
#'
#' @param window A positive whole number giving the number of most recent
#'   scenarios that receive positive probability.
#'
#' @return An object of class `ffp_prior_rolling_window` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' prior_rolling_window(window = 252)
#'
#' model <- ffp_model(c(-0.02, 0.01, 0.03, 0.02))
#' model <- ffp_prior(
#'   model,
#'   prior_rolling_window(window = 2)
#' )
prior_rolling_window <- function(window) {
  call <- rlang::caller_env()

  validate_window(
    window = window,
    call = call
  )

  new_prior_spec(
    method = "rolling_window",
    parameters = list(
      window = as.double(window)
    ),
    class = "ffp_prior_rolling_window"
  )
}


#' Crisp-conditioning prior
#'
#' Creates a prior specification that assigns equal positive probability to
#' scenarios satisfying one or more conditioning expressions and zero
#' probability to all remaining scenarios.
#'
#' Conditioning expressions use data-masking semantics similar to
#' `dplyr::filter()`. Scenario variables can be referenced directly by name,
#' and multiple expressions supplied through `...` are combined with logical
#' AND.
#'
#' Expressions are captured when `prior_crisp()` is called and evaluated only
#' when the specification is applied to an `ffp_model` with [ffp_prior()].
#' This allows expressions to use summaries such as `mean()`, `median()`, and
#' `quantile()`, as well as values from the environment in which the prior was
#' created.
#'
#' Each expression must evaluate to a logical vector of length one or to one
#' logical value per scenario. Missing logical values are not allowed.
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> One or more logical
#'   conditioning expressions. Multiple expressions are combined with logical
#'   AND.
#'
#' @return An object of class `ffp_prior_crisp` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   inflation = c(0.01, 0.02, 0.03, 0.04),
#'   growth = c(0.03, 0.01, -0.01, -0.02)
#' )
#'
#' model <- ffp_model(scenarios)
#'
#' model <- model |>
#'   ffp_prior(
#'     prior_crisp(
#'       inflation >= mean(inflation),
#'       growth < 0
#'     )
#'   )
prior_crisp <- function(...) {
  call <- rlang::caller_env()

  conditions <- rlang::enquos(
    ...,
    .ignore_empty = "none"
  )

  validate_crisp_conditions(
    conditions = conditions,
    call = call
  )

  new_prior_spec(
    method = "crisp",
    parameters = list(
      conditions = conditions
    ),
    class = "ffp_prior_crisp"
  )
}


#' Gaussian kernel-conditioning prior
#'
#' Creates a smooth conditioning prior by assigning greater probability to
#' scenarios whose conditioning variable is closer to a chosen target.
#'
#' `prior_kernel()` currently uses a Gaussian kernel. For a conditioning
#' variable `y`, target `target`, and bandwidth `bandwidth`, the unnormalized
#' weight of scenario `t` is
#'
#' \deqn{
#'   w_t =
#'   \exp\left[
#'     -\frac{1}{2}
#'     \left(
#'       \frac{y_t - \mathrm{target}}{\mathrm{bandwidth}}
#'     \right)^2
#'   \right].
#' }
#'
#' The weights are normalized so that the resulting probabilities sum to one.
#'
#' @section Understanding the target:
#'
#' `target` is the value of the conditioning variable around which probability
#' should be concentrated. Scenarios whose value is closer to `target` receive
#' greater probability than otherwise comparable scenarios farther away.
#'
#' The target uses the same units as the conditioning variable. For example,
#' if `inflation` is represented as a decimal rate, a target inflation rate of
#' 3 percent is specified as `target = 0.03`.
#'
#' The target is the center of the Gaussian kernel. It is not a constraint
#' requiring the weighted mean of the conditioning variable to equal the
#' target exactly.
#'
#' @section Understanding the bandwidth:
#'
#' `bandwidth` controls how broadly probability is distributed around the
#' target. It is expressed in the same units as the conditioning variable and
#' must be strictly positive.
#'
#' A smaller bandwidth concentrates probability more sharply around the
#' target, approaching a crisp conditioning rule as the bandwidth becomes
#' small. A larger bandwidth makes the probability weights flatter and
#' progressively closer to an unconditional uniform distribution.
#'
#' The bandwidth is a smoothing scale, not a probability and not a hard
#' interval around the target. With a Gaussian kernel, a scenario exactly one
#' bandwidth away from the target receives approximately 60.7 percent of the
#' unnormalized weight of a scenario exactly at the target. At two bandwidths
#' away, the relative weight is approximately 13.5 percent.
#'
#' There is no automatic default because the bandwidth is part of the
#' conditioning assumption. It should reflect how narrowly or broadly the
#' user wants to emphasize scenarios around the target.
#'
#' @param variable <[`data-masking`][rlang::args_data_masking]> A numeric
#'   scenario variable or expression that produces exactly one finite numeric
#'   value per scenario.
#' @param target A single finite numeric value giving the center of the
#'   conditioning kernel. It must be expressed in the same units as `variable`.
#' @param bandwidth A single positive finite numeric value controlling the
#'   smoothness of the conditioning weights. It must be expressed in the same
#'   units as `variable`.
#'
#' @return An object of class `ffp_prior_kernel` and `ffp_prior_spec`.
#'
#' @references
#' Meucci, A. (2010). Historical Scenarios with Fully Flexible Probabilities.
#'
#' @seealso [prior_crisp()]
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   inflation = c(0.015, 0.022, 0.029, 0.031, 0.038)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_kernel(
#'       inflation,
#'       target = 0.03,
#'       bandwidth = 0.005
#'     )
#'   )
#'
#' model$prior
prior_kernel <- function(variable, target, bandwidth) {
  call <- rlang::caller_env()
  variable <- rlang::enquo(variable)

  validate_kernel_variable_spec(
    variable = variable,
    call = call
  )

  validate_kernel_target(
    target = target,
    call = call
  )

  validate_kernel_bandwidth(
    bandwidth = bandwidth,
    call = call
  )

  new_prior_spec(
    method = "kernel",
    parameters = list(
      variable = variable,
      target = as.double(target),
      bandwidth = as.double(bandwidth)
    ),
    class = "ffp_prior_kernel"
  )
}


#' Product of prior specifications
#'
#' Combines two or more prior specifications by multiplying their scenario
#' probabilities and normalizing the result.
#'
#' If the component priors produce probability vectors
#' \eqn{p^{(1)}, \ldots, p^{(K)}}, the product prior is
#'
#' \deqn{
#'   p_t =
#'   \frac{
#'     \prod_{k=1}^{K} p_t^{(k)}
#'   }{
#'     \sum_s \prod_{k=1}^{K} p_s^{(k)}
#'   }.
#' }
#'
#' A product prior emphasizes scenarios that receive support from all
#' components simultaneously. If any component assigns zero probability to a
#' scenario, the product also assigns zero probability to that scenario.
#'
#' For example, combining exponential decay with crisp conditioning can be
#' interpreted as emphasizing recent scenarios among those satisfying the
#' conditioning rule.
#'
#' All component specifications are realized against the same `ffp_model`.
#' The component priors must therefore all be compatible with the same
#' scenario support.
#'
#' @param ... Two or more `ffp_prior_spec` objects.
#'
#' @return An object of class `ffp_prior_product` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   inflation = c(0.01, 0.02, 0.03, 0.04)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_product(
#'       prior_exp_decay(half_life = 2),
#'       prior_kernel(
#'         inflation,
#'         target = 0.03,
#'         bandwidth = 0.01
#'       )
#'     )
#'   )
prior_product <- function(...) {
  call <- rlang::caller_env()
  priors <- unname(rlang::list2(...))

  validate_prior_components(
    priors = priors,
    constructor = "prior_product",
    call = call
  )

  new_prior_spec(
    method = "product",
    parameters = list(
      priors = priors
    ),
    class = "ffp_prior_product"
  )
}


#' Mixture of prior specifications
#'
#' Combines two or more prior specifications as a weighted mixture.
#'
#' If the component priors produce probability vectors
#' \eqn{p^{(1)}, \ldots, p^{(K)}} and the mixture weights are
#' \eqn{w_1, \ldots, w_K}, the resulting prior is
#'
#' \deqn{
#'   p_t = \sum_{k=1}^{K} w_k p_t^{(k)}.
#' }
#'
#' The weights must be non-negative and sum to one. They are matched to the
#' component priors by position.
#'
#' Unlike [prior_product()], a mixture does not require all components to
#' support the same scenarios positively. A scenario receiving zero
#' probability from one component can still receive positive probability in
#' the mixture through another component.
#'
#' Mixture weights are required explicitly because they are part of the
#' modelling assumption. No equal-weight default is imposed automatically.
#'
#' @param ... Two or more `ffp_prior_spec` objects.
#' @param weights A numeric vector of non-negative mixture weights, with one
#'   value per component prior, summing to one. Weights are matched to
#'   component priors by position.
#'
#' @return An object of class `ffp_prior_mixture` and `ffp_prior_spec`.
#'
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   inflation = c(0.01, 0.02, 0.03, 0.04)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_mixture(
#'       prior_exp_decay(half_life = 2),
#'       prior_kernel(
#'         inflation,
#'         target = 0.03,
#'         bandwidth = 0.01
#'       ),
#'       weights = c(0.7, 0.3)
#'     )
#'   )
prior_mixture <- function(..., weights) {
  call <- rlang::caller_env()
  priors <- unname(rlang::list2(...))

  validate_prior_components(
    priors = priors,
    constructor = "prior_mixture",
    call = call
  )

  if (missing(weights)) {
    ffp_abort(
      c(
        "{.fn prior_mixture} requires explicit {.arg weights}.",
        "i" = paste0(
          "Supply one non-negative weight per component prior, with the ",
          "weights summing to 1."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  weights <- validate_mixture_weights(
    weights = weights,
    n_priors = length(priors),
    call = call
  )

  new_prior_spec(
    method = "mixture",
    parameters = list(
      priors = priors,
      weights = weights
    ),
    class = "ffp_prior_mixture"
  )
}


# Apply prior -------------------------------------------------------------

#' Set the prior distribution of an FFP model
#'
#' Applies a prior specification to the scenario support of an `ffp_model`.
#'
#' A prior specification is realized against the model, validated, and stored
#' together with the resulting probability vector. Applying a new prior
#' replaces any prior previously attached to the model.
#'
#' @param model An `ffp_model`.
#' @param prior An `ffp_prior_spec`, such as one created by [prior_uniform()],
#'   [prior_custom()], [prior_exp_decay()], [prior_rolling_window()],
#'   [prior_crisp()], [prior_kernel()], [prior_product()], or
#'   [prior_mixture()].
#'
#' @return An updated `ffp_model`.
#'
#' @export
#'
#' @examples
#' model <- ffp_model(c(-0.02, 0.01, 0.03))
#'
#' model <- model |>
#'   ffp_prior(
#'     prior_uniform()
#'   )
#'
#' model$prior
ffp_prior <- function(model, prior) {
  call <- rlang::caller_env()

  validate_ffp_model(model, call = call)
  validate_prior_spec(prior, call = call)

  probabilities <- realize_prior(
    prior = prior,
    model = model,
    call = call
  )

  probabilities <- validate_realized_prior(
    probabilities = probabilities,
    model = model,
    call = call
  )

  model$prior <- probabilities
  model$prior_spec <- prior

  model
}


# Prior realization -------------------------------------------------------

#' @noRd
realize_prior <- function(prior, model, call = rlang::caller_env()) {
  UseMethod("realize_prior")
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_uniform <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  n_scenarios <- scenario_count(model$scenarios)

  rep(1 / n_scenarios, n_scenarios)
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_custom <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  prior$parameters$probabilities
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_exp_decay <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  validate_temporal_scenario_order(
    model = model,
    call = call
  )

  exp_decay_probabilities(
    n_scenarios = scenario_count(model$scenarios),
    half_life = prior$parameters$half_life,
    call = call
  )
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_rolling_window <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  validate_temporal_scenario_order(
    model = model,
    call = call
  )

  n_scenarios <- scenario_count(model$scenarios)
  window <- prior$parameters$window

  validate_window_compatibility(
    window = window,
    n_scenarios = n_scenarios,
    call = call
  )

  rolling_window_probabilities(
    n_scenarios = n_scenarios,
    window = window
  )
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_crisp <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  n_scenarios <- scenario_count(model$scenarios)
  data <- scenario_data_mask(model$scenarios)

  selected <- rep(TRUE, n_scenarios)

  for (i in seq_along(prior$parameters$conditions)) {
    condition <- prior$parameters$conditions[[i]]

    result <- evaluate_crisp_condition(
      condition = condition,
      data = data,
      n_scenarios = n_scenarios,
      condition_number = i,
      call = call
    )

    selected <- selected & result
  }

  if (!any(selected)) {
    ffp_abort(
      c(
        "The crisp conditioning set is empty.",
        "x" = "No scenario satisfies all conditioning expressions.",
        "i" = paste0(
          "Adjust the conditions so that at least one scenario receives ",
          "positive probability."
        )
      ),
      class = c(
        "ffp_error_empty_conditioning_set",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  crisp_conditioning_probabilities(selected)
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_kernel <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  n_scenarios <- scenario_count(model$scenarios)
  data <- scenario_data_mask(model$scenarios)

  values <- evaluate_kernel_variable(
    variable = prior$parameters$variable,
    data = data,
    n_scenarios = n_scenarios,
    call = call
  )

  kernel_conditioning_probabilities(
    values = values,
    target = prior$parameters$target,
    bandwidth = prior$parameters$bandwidth,
    call = call
  )
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_product <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  probabilities <- lapply(
    prior$parameters$priors,
    realize_prior_component,
    model = model,
    call = call
  )

  prior_product_probabilities(
    probabilities = probabilities,
    call = call
  )
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_mixture <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  probabilities <- lapply(
    prior$parameters$priors,
    realize_prior_component,
    model = model,
    call = call
  )

  prior_mixture_probabilities(
    probabilities = probabilities,
    weights = prior$parameters$weights
  )
}


#' @exportS3Method
#' @noRd
realize_prior.ffp_prior_spec <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  ffp_abort(
    c(
      "Unsupported prior specification.",
      "x" = paste0(
        "No realization method is available for class `",
        class(prior)[[1]],
        "`."
      )
    ),
    class = "ffp_error_invalid_prior",
    call = call
  )
}


#' @exportS3Method
#' @noRd
realize_prior.default <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  ffp_abort(
    c(
      "Unsupported prior specification.",
      "x" = "{.arg prior} must inherit from {.cls ffp_prior_spec}."
    ),
    class = "ffp_error_invalid_prior",
    call = call
  )
}


realize_prior_component <- function(
    prior,
    model,
    call = rlang::caller_env()
) {
  validate_prior_spec(
    prior = prior,
    call = call
  )

  probabilities <- realize_prior(
    prior = prior,
    model = model,
    call = call
  )

  validate_realized_prior(
    probabilities = probabilities,
    model = model,
    call = call
  )
}


# Prior computation -------------------------------------------------------

exp_decay_probabilities <- function(
    n_scenarios,
    half_life,
    call = rlang::caller_env()
) {
  ages <- n_scenarios - seq_len(n_scenarios)
  log_weights <- -log(2) * ages / half_life

  normalize_log_weights(
    log_weights = log_weights,
    context = "exponential-decay prior",
    call = call
  )
}


rolling_window_probabilities <- function(n_scenarios, window) {
  probabilities <- numeric(n_scenarios)

  first_active <- n_scenarios - window + 1
  active <- seq.int(first_active, n_scenarios)

  probabilities[active] <- 1 / window

  probabilities
}


crisp_conditioning_probabilities <- function(selected) {
  weights <- as.double(selected)

  weights / sum(weights)
}


kernel_conditioning_probabilities <- function(
    values,
    target,
    bandwidth,
    call = rlang::caller_env()
) {
  scaled_distance <- (values - target) / bandwidth
  log_weights <- -0.5 * scaled_distance^2

  normalize_log_weights(
    log_weights = log_weights,
    context = "kernel-conditioning prior",
    call = call
  )
}


prior_product_probabilities <- function(
    probabilities,
    call = rlang::caller_env()
) {
  probability_matrix <- do.call(
    cbind,
    probabilities
  )

  structural_zero <- apply(
    probability_matrix == 0,
    1,
    any
  )

  if (all(structural_zero)) {
    ffp_abort(
      c(
        "The component priors have no common positive support.",
        "x" = paste0(
          "Every scenario receives zero probability from at least one ",
          "component prior."
        ),
        "i" = paste0(
          "A product prior requires at least one scenario with positive ",
          "probability in every component."
        )
      ),
      class = c(
        "ffp_error_empty_prior_product_support",
        "ffp_error_incompatible_prior"
      ),
      call = call
    )
  }

  log_probabilities <- log(probability_matrix)

  log_weights <- rowSums(
    log_probabilities
  )

  normalize_log_weights(
    log_weights = log_weights,
    structural_zero = structural_zero,
    context = "product prior",
    call = call
  )
}


prior_mixture_probabilities <- function(probabilities, weights) {
  probability_matrix <- do.call(
    cbind,
    probabilities
  )

  mixture <- as.vector(
    probability_matrix %*% weights
  )

  canonicalize_probabilities(mixture)
}


normalize_log_weights <- function(
    log_weights,
    structural_zero = rep(FALSE, length(log_weights)),
    context = "prior",
    call = rlang::caller_env()
) {
  active <- !structural_zero
  finite_active <- active & is.finite(log_weights)

  if (!any(finite_active)) {
    ffp_abort(
      c(
        paste0("The ", context, " weights could not be computed reliably."),
        "x" = "No finite log-weight is available for normalization.",
        "i" = paste0(
          "Check the prior parameters and the scale of the scenario ",
          "variables."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  max_log_weight <- max(
    log_weights[finite_active]
  )

  shifted <- log_weights - max_log_weight

  min_log_weight <- log(
    .Machine$double.xmin
  )

  non_finite_active <- active & !is.finite(shifted)

  shifted[non_finite_active] <- min_log_weight

  shifted[active] <- pmax(
    shifted[active],
    min_log_weight
  )

  weights <- numeric(
    length(log_weights)
  )

  weights[active] <- exp(
    shifted[active]
  )

  total <- sum(weights)

  if (!is.finite(total) || total <= 0) {
    ffp_abort(
      c(
        paste0("The ", context, " weights could not be normalized."),
        "i" = "Check the prior parameters and scenario support."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  weights / total
}


# Crisp conditioning ------------------------------------------------------

evaluate_crisp_condition <- function(
    condition,
    data,
    n_scenarios,
    condition_number,
    call = rlang::caller_env()
) {
  condition_label <- rlang::as_label(condition)

  result <- tryCatch(
    rlang::eval_tidy(
      condition,
      data = data
    ),
    error = function(error) {
      ffp_abort(
        c(
          "Failed to evaluate a crisp conditioning expression.",
          "x" = paste0(
            "Condition ",
            condition_number,
            ": ",
            condition_label
          ),
          "i" = paste0(
            "Conditions are evaluated against the scenario variables and ",
            "the environment in which the prior was created."
          )
        ),
        class = c(
          "ffp_error_invalid_prior_condition",
          "ffp_error_invalid_prior"
        ),
        parent = error,
        call = call
      )
    }
  )

  validate_crisp_condition_result(
    result = result,
    n_scenarios = n_scenarios,
    condition_number = condition_number,
    condition_label = condition_label,
    call = call
  )
}


validate_crisp_condition_result <- function(
    result,
    n_scenarios,
    condition_number,
    condition_label,
    call = rlang::caller_env()
) {
  if (!is.logical(result) || !is.null(dim(result))) {
    ffp_abort(
      c(
        "A crisp conditioning expression must return logical values.",
        "x" = paste0(
          "Condition ",
          condition_number,
          " did not return a logical vector: ",
          condition_label
        ),
        "i" = paste0(
          "Each condition must identify which scenarios satisfy the ",
          "conditioning rule."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  result_size <- length(result)

  if (!(result_size %in% c(1L, n_scenarios))) {
    ffp_abort(
      c(
        "A crisp conditioning expression has an incompatible size.",
        "x" = paste0(
          "Condition ",
          condition_number,
          " returned ",
          result_size,
          " value(s), but the model contains ",
          n_scenarios,
          " scenario(s)."
        ),
        "i" = paste0(
          "Each condition must return either one logical value or one ",
          "logical value per scenario."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  if (anyNA(result)) {
    ffp_abort(
      c(
        "A crisp conditioning expression produced missing values.",
        "x" = paste0(
          "Condition ",
          condition_number,
          " contains at least one {.val NA} result."
        ),
        "i" = paste0(
          "Every scenario must resolve unambiguously to {.val TRUE} or ",
          "{.val FALSE}."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  if (result_size == 1L) {
    result <- rep(
      result,
      n_scenarios
    )
  }

  unname(result)
}


# Kernel conditioning -----------------------------------------------------

evaluate_kernel_variable <- function(
    variable,
    data,
    n_scenarios,
    call = rlang::caller_env()
) {
  variable_label <- rlang::as_label(variable)

  values <- tryCatch(
    rlang::eval_tidy(
      variable,
      data = data
    ),
    error = function(error) {
      ffp_abort(
        c(
          "Failed to evaluate the kernel conditioning variable.",
          "x" = paste0(
            "Could not evaluate: ",
            variable_label
          ),
          "i" = paste0(
            "The conditioning variable is evaluated against the scenario ",
            "variables and the environment in which the prior was created."
          )
        ),
        class = c(
          "ffp_error_invalid_prior_condition",
          "ffp_error_invalid_prior"
        ),
        parent = error,
        call = call
      )
    }
  )

  validate_kernel_variable_result(
    values = values,
    n_scenarios = n_scenarios,
    variable_label = variable_label,
    call = call
  )

  unname(
    as.double(values)
  )
}


validate_kernel_variable_result <- function(
    values,
    n_scenarios,
    variable_label,
    call = rlang::caller_env()
) {
  if (!is.numeric(values) || !is.null(dim(values))) {
    ffp_abort(
      c(
        "The kernel conditioning variable must be numeric.",
        "x" = paste0(
          "`",
          variable_label,
          "` did not evaluate to a numeric vector."
        ),
        "i" = paste0(
          "Gaussian kernel conditioning requires a quantitative variable ",
          "with one value per scenario."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  if (length(values) != n_scenarios) {
    ffp_abort(
      c(
        "The kernel conditioning variable has an incompatible size.",
        "x" = paste0(
          "`",
          variable_label,
          "` returned ",
          length(values),
          " value(s), but the model contains ",
          n_scenarios,
          " scenario(s)."
        ),
        "i" = "The conditioning variable must provide one value per scenario."
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  if (anyNA(values)) {
    ffp_abort(
      c(
        "The kernel conditioning variable must not contain missing values.",
        "x" = paste0(
          "`",
          variable_label,
          "` produced at least one {.val NA} or {.val NaN} value."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  if (any(!is.finite(values))) {
    ffp_abort(
      c(
        "The kernel conditioning variable must contain only finite values.",
        "x" = paste0(
          "`",
          variable_label,
          "` produced at least one {.val Inf} or {.val -Inf} value."
        )
      ),
      class = c(
        "ffp_error_invalid_prior_condition",
        "ffp_error_invalid_prior"
      ),
      call = call
    )
  }

  invisible(values)
}


scenario_data_mask <- function(scenarios) {
  variable_names <- scenario_variable_names(scenarios)

  if (is.null(variable_names)) {
    return(list())
  }

  unnamed <- is.na(variable_names) | !nzchar(variable_names)

  if (all(unnamed)) {
    return(list())
  }

  if (inherits(scenarios, "data.frame")) {
    return(
      as.list(scenarios)
    )
  }

  values <- as.matrix(scenarios)

  columns <- lapply(
    seq_len(NCOL(values)),
    function(i) values[, i]
  )

  names(columns) <- variable_names

  columns
}


# Validation --------------------------------------------------------------

validate_ffp_model <- function(
    model,
    call = rlang::caller_env()
) {
  if (!inherits(model, "ffp_model")) {
    ffp_abort(
      c(
        "{.arg model} must be an {.cls ffp_model} object.",
        "i" = "Create a model with {.fn ffp_model} before defining a prior."
      ),
      class = "ffp_error_invalid_model",
      call = call
    )
  }

  invisible(model)
}


validate_prior_spec <- function(
    prior,
    call = rlang::caller_env()
) {
  if (!inherits(prior, "ffp_prior_spec")) {
    ffp_abort(
      c(
        "{.arg prior} must be an {.cls ffp_prior_spec} object.",
        "i" = paste0(
          "Create a prior specification with a prior constructor such as ",
          "{.fn prior_uniform}, {.fn prior_custom}, {.fn prior_exp_decay}, ",
          "{.fn prior_rolling_window}, {.fn prior_crisp}, {.fn prior_kernel}, ",
          "{.fn prior_product}, or {.fn prior_mixture}."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(prior)
}


validate_prior_components <- function(
    priors,
    constructor,
    call = rlang::caller_env()
) {
  if (length(priors) < 2L) {
    ffp_abort(
      c(
        paste0(
          "{.fn ",
          constructor,
          "} requires at least two prior specifications."
        ),
        "i" = "Supply two or more {.cls ffp_prior_spec} objects."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  valid <- vapply(
    priors,
    inherits,
    logical(1),
    what = "ffp_prior_spec"
  )

  if (!all(valid)) {
    invalid <- which(!valid)[[1]]

    ffp_abort(
      c(
        paste0(
          "All components of {.fn ",
          constructor,
          "} must be {.cls ffp_prior_spec} objects."
        ),
        "x" = paste0(
          "Component ",
          invalid,
          " has class `",
          class(priors[[invalid]])[[1]],
          "`."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(priors)
}


validate_half_life <- function(
    half_life,
    call = rlang::caller_env()
) {
  if (
    !is.numeric(half_life) ||
    length(half_life) != 1L ||
    !is.null(dim(half_life)) ||
    is.na(half_life) ||
    !is.finite(half_life) ||
    half_life <= 0
  ) {
    ffp_abort(
      c(
        "{.arg half_life} must be a positive finite number.",
        "i" = "The half-life is measured in number of observations."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(half_life)
}


validate_window <- function(
    window,
    call = rlang::caller_env()
) {
  if (
    !is.numeric(window) ||
    length(window) != 1L ||
    !is.null(dim(window)) ||
    is.na(window) ||
    !is.finite(window) ||
    window <= 0 ||
    window != floor(window)
  ) {
    ffp_abort(
      c(
        "{.arg window} must be a positive whole number.",
        "i" = "The rolling window is measured in number of observations."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(window)
}


validate_window_compatibility <- function(
    window,
    n_scenarios,
    call = rlang::caller_env()
) {
  if (window > n_scenarios) {
    ffp_abort(
      c(
        "The rolling-window prior is incompatible with the scenario support.",
        "x" = paste0(
          "The requested window contains ",
          format(window, scientific = FALSE, trim = TRUE),
          " observation(s), but the model contains only ",
          format(n_scenarios, scientific = FALSE, trim = TRUE),
          " scenario(s)."
        ),
        "i" = "Choose a window no larger than the number of scenarios."
      ),
      class = "ffp_error_incompatible_prior",
      call = call
    )
  }

  invisible(window)
}


validate_crisp_conditions <- function(
    conditions,
    call = rlang::caller_env()
) {
  if (length(conditions) == 0L) {
    ffp_abort(
      c(
        "{.fn prior_crisp} requires at least one conditioning expression.",
        "i" = paste0(
          "Supply one or more logical expressions identifying the ",
          "scenarios to retain."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  missing_conditions <- vapply(
    conditions,
    rlang::quo_is_missing,
    logical(1)
  )

  if (any(missing_conditions)) {
    ffp_abort(
      c(
        "Crisp conditioning expressions must not be empty.",
        "i" = "Remove empty arguments or supply a logical expression."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(conditions)
}


validate_kernel_variable_spec <- function(
    variable,
    call = rlang::caller_env()
) {
  if (rlang::quo_is_missing(variable)) {
    ffp_abort(
      c(
        "{.fn prior_kernel} requires a conditioning variable.",
        "i" = paste0(
          "Supply a numeric scenario variable or expression to be ",
          "conditioned around the target."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(variable)
}


validate_kernel_target <- function(
    target,
    call = rlang::caller_env()
) {
  if (
    !is.numeric(target) ||
    length(target) != 1L ||
    !is.null(dim(target)) ||
    is.na(target) ||
    !is.finite(target)
  ) {
    ffp_abort(
      c(
        "{.arg target} must be a single finite number.",
        "i" = paste0(
          "The target is the value of the conditioning variable around ",
          "which probability is concentrated."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(target)
}


validate_kernel_bandwidth <- function(
    bandwidth,
    call = rlang::caller_env()
) {
  if (
    !is.numeric(bandwidth) ||
    length(bandwidth) != 1L ||
    !is.null(dim(bandwidth)) ||
    is.na(bandwidth) ||
    !is.finite(bandwidth) ||
    bandwidth <= 0
  ) {
    ffp_abort(
      c(
        "{.arg bandwidth} must be a single positive finite number.",
        "i" = paste0(
          "The bandwidth controls how broadly probability is distributed ",
          "around the target."
        ),
        "i" = paste0(
          "Use the same units for {.arg bandwidth}, {.arg target}, and ",
          "the conditioning variable."
        )
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(bandwidth)
}


validate_mixture_weights <- function(
    weights,
    n_priors,
    call = rlang::caller_env()
) {
  if (length(weights) != n_priors) {
    ffp_abort(
      c(
        "{.arg weights} must contain one value per component prior.",
        "x" = paste0(
          "Received ",
          length(weights),
          " weight(s) for ",
          n_priors,
          " component prior(s)."
        ),
        "i" = "Mixture weights are matched to component priors by position."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  validate_probability_vector(
    probabilities = weights,
    arg = "weights",
    call = call
  )

  if (!is.null(names(weights))) {
    cli::cli_warn(
      c(
        "Names in {.arg weights} are ignored.",
        "i" = "Mixture weights are matched to component priors by position."
      ),
      class = c(
        "ffp_warning_named_mixture_weights",
        "ffp_warning"
      ),
      call = call
    )
  }

  canonicalize_probabilities(weights)
}


validate_probability_vector <- function(
    probabilities,
    arg,
    call = rlang::caller_env()
) {
  if (!is.numeric(probabilities) || !is.null(dim(probabilities))) {
    ffp_abort(
      c(
        paste0("`", arg, "` must be a numeric vector."),
        "x" = "Matrices, arrays, and non-numeric objects are not supported."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  if (length(probabilities) == 0L) {
    ffp_abort(
      paste0("`", arg, "` must contain at least one probability."),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  if (anyNA(probabilities)) {
    ffp_abort(
      c(
        paste0("`", arg, "` must not contain missing values."),
        "x" = "Found at least one {.val NA} or {.val NaN} value."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  if (any(!is.finite(probabilities))) {
    ffp_abort(
      c(
        paste0("`", arg, "` must contain only finite values."),
        "x" = "Found at least one {.val Inf} or {.val -Inf} value."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  if (any(probabilities < 0)) {
    ffp_abort(
      c(
        paste0("`", arg, "` must be non-negative."),
        "x" = "Probabilities smaller than zero are not valid."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  total <- sum(probabilities)

  if (!probability_sum_is_one(total)) {
    ffp_abort(
      c(
        paste0("`", arg, "` must sum to 1."),
        "x" = paste0(
          "Current sum is ",
          format(total, digits = 15, scientific = FALSE, trim = TRUE),
          "."
        ),
        "i" = "Probabilities are not normalized automatically."
      ),
      class = "ffp_error_invalid_prior",
      call = call
    )
  }

  invisible(probabilities)
}


validate_realized_prior <- function(
    probabilities,
    model,
    call = rlang::caller_env()
) {
  n_scenarios <- scenario_count(model$scenarios)

  if (length(probabilities) != n_scenarios) {
    ffp_abort(
      c(
        "The prior is incompatible with the scenario support.",
        "x" = paste0(
          "The prior contains ",
          length(probabilities),
          " probability value(s), but the model contains ",
          n_scenarios,
          " scenario(s)."
        ),
        "i" = "A prior must provide exactly one probability per scenario."
      ),
      class = "ffp_error_incompatible_prior",
      call = call
    )
  }

  validate_probability_vector(
    probabilities = probabilities,
    arg = "prior",
    call = call
  )

  canonicalize_probabilities(probabilities)
}


validate_temporal_scenario_order <- function(
    model,
    call = rlang::caller_env()
) {
  index <- model$metadata$index

  if (
    is.null(index) ||
    !is_temporal_index(index) ||
    length(index) <= 1L
  ) {
    return(
      invisible(model)
    )
  }

  index_numeric <- as.numeric(index)

  if (any(diff(index_numeric) < 0)) {
    ffp_abort(
      c(
        "The temporal scenario index must be ordered from oldest to newest.",
        "x" = "The current index decreases at least once.",
        "i" = paste0(
          "Time-sensitive priors assign special meaning to the order of ",
          "scenarios."
        ),
        "i" = "Reorder the scenario data before applying the prior."
      ),
      class = "ffp_error_invalid_scenario_order",
      call = call
    )
  }

  invisible(model)
}


is_temporal_index <- function(index) {
  inherits(index, "Date") || inherits(index, "POSIXt")
}


probability_sum_is_one <- function(total) {
  tolerance <- sqrt(.Machine$double.eps)

  abs(total - 1) <= tolerance
}


canonicalize_probabilities <- function(probabilities) {
  probabilities <- unname(
    as.double(probabilities)
  )

  probabilities / sum(probabilities)
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_prior_spec <- function(x, ...) {
  cat("<ffp_prior_spec>\n\n")

  cat(
    "Method: ",
    format_prior_method(x$method),
    "\n",
    sep = ""
  )

  if (identical(x$method, "custom")) {
    cat(
      "Probabilities: ",
      length(x$parameters$probabilities),
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "exp_decay")) {
    cat(
      "Half-life: ",
      format(
        x$parameters$half_life,
        scientific = FALSE,
        trim = TRUE
      ),
      " observations\n",
      sep = ""
    )
  }

  if (identical(x$method, "rolling_window")) {
    cat(
      "Window: ",
      format(
        x$parameters$window,
        scientific = FALSE,
        trim = TRUE
      ),
      " observations\n",
      sep = ""
    )
  }

  if (identical(x$method, "crisp")) {
    cat(
      "Conditions: ",
      length(x$parameters$conditions),
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "kernel")) {
    cat(
      "Variable: ",
      rlang::as_label(x$parameters$variable),
      "\n",
      sep = ""
    )

    cat(
      "Target: ",
      format_prior_number(x$parameters$target),
      "\n",
      sep = ""
    )

    cat(
      "Bandwidth: ",
      format_prior_number(x$parameters$bandwidth),
      "\n",
      sep = ""
    )
  }

  if (identical(x$method, "product")) {
    print_prior_components(
      x$parameters$priors
    )
  }

  if (identical(x$method, "mixture")) {
    print_prior_components(
      x$parameters$priors,
      weights = x$parameters$weights
    )
  }

  invisible(x)
}


print_prior_components <- function(priors, weights = NULL) {
  cat(
    "Components: ",
    length(priors),
    "\n",
    sep = ""
  )

  for (i in seq_along(priors)) {
    cat(
      "  ",
      i,
      ". ",
      format_prior_method(priors[[i]]$method),
      sep = ""
    )

    if (!is.null(weights)) {
      cat(
        " [weight: ",
        format_prior_number(weights[[i]]),
        "]",
        sep = ""
      )
    }

    cat("\n")
  }

  invisible(NULL)
}


format_prior_method <- function(method) {
  switch(
    method,
    uniform = "uniform",
    custom = "custom",
    exp_decay = "exponential decay",
    rolling_window = "rolling window",
    crisp = "crisp conditioning",
    kernel = "kernel conditioning",
    product = "product",
    mixture = "mixture",
    method
  )
}


format_prior_number <- function(x) {
  format(
    x,
    digits = 15,
    scientific = FALSE,
    trim = TRUE
  )
}
