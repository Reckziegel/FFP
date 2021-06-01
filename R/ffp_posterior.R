ffp_posterior <- function(x, p) {
  mu = t(x) %*% p
  Scnd_Mom = t(x) %*% (x * (p %*% matrix(1, 1, NCOL(x))))
  Scnd_Mom = (Scnd_Mom + t(Scnd_Mom)) / 2
  sigma = Scnd_Mom - mu %*% t(mu)
  list(mu = mu, sigma = sigma)
}
