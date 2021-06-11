pHist <- function(X, p) {
  nBins <- round(10 * log(NROW(X)))
  tmp <- graphics::hist(X, seq(min(X), max(X), length.out = nBins + 1), plot = FALSE)
  x <- tmp$mids
  n <- tmp$counts

  D  <-  x[2] - x[1]
  N  <- vctrs::vec_size(x)
  np <- vector("double", N)
  for (s in 1:N) {
    Index <- (X >= x[s] - D / 2) & (X <= x[s] + D / 2)
    np[s] <- sum(p[Index])
    #f <- np / D
    f = np
  }
  cbind(x, f)
}


# pnl <- rnorm(1000)
# p <- crisp(pnl, pnl < 0.005)
# p <- smoothing(pnl, 0.01)

# flexible_density <- function(x, p) {
#   .size <- vctrs::vec_size(x)
#
#   scenarios_conditional <- bootstrap_scenarios(x, p, 10000) %>% as.vector()
#   mean_conditional <- mean(scenarios_conditional, na.rm = TRUE)
#
#   scenarios_unconditional <- bootstrap_scenarios(x, rep(1 / .size, .size), 10000) %>%
#     as.vector()
#   mean_unconditional <- mean(scenarios_unconditional, na.rm = TRUE)
#
#   tib_cond  <- tibble(.pnl  = scenarios_conditional, scenario = as.factor("conditional"))
#   tib_uncon <- tibble(.pnl = scenarios_unconditional, scenario = as.factor("unconditional"))
#   bind_rows(tib_cond, tib_uncon) %>%
#     ggplot(aes(x = .pnl, y = after_stat(density), fill = scenario, color = scenario)) +
#     geom_density(alpha= 0.5) +
#     geom_point(aes(x = mean_conditional, y = 0), color = "red", size = 3) +
#     geom_point(aes(x = mean_unconditional, y = 0), color = "blue", size = 3) +
#     #facet_wrap(~ scenario, ncol = 1) +
#     scale_y_continuous(labels = scales::percent_format(scale = 10000)) +
#     labs(x = NULL, y = NULL)
# }
