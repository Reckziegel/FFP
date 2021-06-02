% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_scenarios.R
\name{bootstrap_scenarios}
\alias{bootstrap_scenarios}
\alias{bootstrap_scenarios.numeric}
\alias{bootstrap_scenarios.matrix}
\alias{bootstrap_scenarios.ts}
\alias{bootstrap_scenarios.xts}
\alias{bootstrap_scenarios.tbl}
\alias{bootstrap_scenarios.data.frame}
\title{Flexible Probabilities Driven Bootstrap}
\usage{
bootstrap_scenarios(x, p, n)

\method{bootstrap_scenarios}{numeric}(x, p, n)

\method{bootstrap_scenarios}{matrix}(x, p, n)

\method{bootstrap_scenarios}{ts}(x, p, n)

\method{bootstrap_scenarios}{xts}(x, p, n)

\method{bootstrap_scenarios}{tbl}(x, p, n)

\method{bootstrap_scenarios}{data.frame}(x, p, n)
}
\arguments{
\item{x}{A time-series defining the scenario-probability distribution.}

\item{p}{A vector of probabilities.}

\item{n}{An \code{integer} scalar with the number of scenarios to be generated.}
}
\value{
The argument `x` is supposed to have the same size of `p`.
}
\description{
This function resamples scenarios given by the flexible probabilities
approach.
}
\examples{
set.seed(123)
ret <- diff(log(EuStockMarkets))
ew  <- rep(1 / nrow(ret), nrow(ret))

bootstrap_scenarios(x = ret, p = ew, n = 10)
}