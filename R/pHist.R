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
    f <- np / D
  }
  cbind(x, f)
}

