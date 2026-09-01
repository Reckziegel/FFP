
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/ffp_sticker.png" align="right" width="147" height="170"/>

# Fully Flexible Probabilities

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Reckziegel/FFP/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/FFP/actions)
[![Codecov test
coverage](https://codecov.io/gh/Reckziegel/FFP/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Reckziegel/FFP?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ffp)](https://CRAN.R-project.org/package=ffp)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/ffp?color=blue)](https://r-pkg.org/pkg/ffp)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ffp?color=blue)](https://r-pkg.org/pkg/ffp)

<!-- badges: end -->

> **Flexible scenario probabilities for conditioning, stress testing,
> and portfolio analysis in R.**

`ffp` provides tools for assigning flexible probabilities to historical
or simulated scenarios and for updating those probabilities when new
information or subjective market views become available.

The central idea is simple:

> **Keep the scenarios, change their probabilities.**

Instead of replacing an existing scenario set, `ffp` changes how much
probability is assigned to each observation.

Flexible probabilities can be used to:

- emphasize recent observations;
- condition the historical sample on a particular market state;
- incorporate partial information;
- impose subjective views through Entropy Pooling;
- carry the resulting probabilities into risk measurement, simulation,
  and portfolio analysis.

Conceptually,

$$
\text{scenarios}
+
\text{information}
\longrightarrow
\text{flexible probabilities}
$$

The scenarios remain available throughout the analysis. What changes is
how important each scenario is.

## Installation

Install the released version from CRAN with:

``` r
install.packages("ffp")
```

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/FFP")
```

## Quick start

A small example illustrates the basic workflow.

Suppose the historical scenarios are the daily log returns in
`EuStockMarkets`.

``` r
library(ffp)

x <- diff(log(EuStockMarkets))

prior <- rep(1 / nrow(x), nrow(x))
```

The matrix `x` contains the historical scenarios, while `prior` assigns
the same probability to every observation.

Now suppose the investor believes that the expected return of the `FTSE`
should be 20% above its historical average.

``` r
target <- mean(x[, "FTSE"]) * 1.20

view <- view_on_mean(
  x = x[, "FTSE", drop = FALSE],
  mean = target
)
```

The view is incorporated through Entropy Pooling:

``` r
posterior <- entropy_pooling(
  p = prior,
  Aeq = view$Aeq,
  beq = view$beq, 
  solver = "nlminb"
)
```

We can check the resulting expected returns with:

``` r
ffp_moments(x, posterior)$mu
#>          DAX          SMI          CAC         FTSE 
#> 0.0007233428 0.0008764380 0.0005145713 0.0005183822
```

The important point is that the observations in `x` have **not**
changed.

Entropy Pooling has only changed their probabilities.

In symbols,

$$
\underbrace{(X,p)}_{\text{prior}}
\quad\longrightarrow\quad
\underbrace{(X,q^*)}_{\text{posterior}},
$$

where:

- $X$ contains the historical or simulated scenarios;
- $p$ contains the prior probabilities;
- $q^*$ contains the posterior probabilities.

This simple idea is at the center of the package:

$$
\text{same scenarios, different probabilities}
$$

## How `ffp` fits together

There are two main ways of introducing information into scenario
probabilities.

### Conditioning

Sometimes the information comes from the data itself.

For example, we may want recent observations to receive more weight, or
we may want to condition the historical sample on a particular economic
state.

### Views and stress testing

In other situations, the information comes from a subjective view or
stress scenario.

Both approaches ultimately produce probability weights over the same
scenario set.

These probabilities can then be used for risk measurement, simulation,
or portfolio analysis.

## Constructing flexible probabilities

Flexible probabilities allow different observations to carry different
amounts of information.

The appropriate function depends on the type of information available.

| Information available                      | Function           |
|:-------------------------------------------|:-------------------|
| Recent observations should matter more     | `exp_decay()`      |
| Condition on a hard event                  | `crisp()`          |
| Condition smoothly around a target         | `kernel_normal()`  |
| Impose partial information through entropy | `kernel_entropy()` |
| Combine fast and slow decay information    | `double_decay()`   |

For example, exponential decay gives progressively more weight to recent
observations:

``` r
p <- exp_decay(x, 0.01)

p
#> <ffp[1859]>
#> 8.484746e-11 8.57002e-11 8.65615e-11 8.743145e-11 8.831015e-11 ... 0.009950166
```

The output is a probability vector associated with the historical
scenarios.

The probabilities do not need to be uniform: they reflect the
information that the user wants the historical sample to represent.

## Stress testing with Entropy Pooling

Entropy Pooling allows subjective market views to be incorporated by
reweighting the scenarios.

The `view_on_*()` functions translate an investor’s statement into the
constraints required by `entropy_pooling()`.

| If your view is about…          | Use                               |
|:--------------------------------|:----------------------------------|
| Expected returns or means       | `view_on_mean()`                  |
| Volatility                      | `view_on_volatility()`            |
| Covariance                      | `view_on_covariance()`            |
| Correlation                     | `view_on_correlation()`           |
| Relative performance or ranking | `view_on_rank()`                  |
| Marginal distributions          | `view_on_marginal_distribution()` |
| Dependence or copulas           | `view_on_copula()`                |
| A joint target distribution     | `view_on_joint_distribution()`    |

A useful way to think about the API is:

$$
\mathtt{view\_on\_*()}
\longrightarrow
\mathtt{entropy\_pooling()}
$$

For several simultaneous views,

$$
\texttt{view\_on\_*()}
\longrightarrow
\texttt{bind\_views()}
\longrightarrow
\texttt{entropy\_pooling()}
$$

For example:

``` r
view_mean <- view_on_mean(
  x = x[, "FTSE", drop = FALSE],
  mean = mean(x[, "FTSE"]) * 1.20
)

view_vol <- view_on_volatility(
  x = x[, "CAC", drop = FALSE],
  vol = sd(x[, "CAC"]) * 0.90
)

views <- bind_views(
  view_mean,
  view_vol
)

posterior <- entropy_pooling(
  p = prior,
  Aeq = views$Aeq,
  beq = views$beq,
  A = views$A,
  b = views$b, 
  solver = "nlminb"
)
```

The separation between these functions is intentional:

- `view_on_*()` describes **what the investor believes**;
- `bind_views()` combines several beliefs;
- `entropy_pooling()` determines **how scenario probabilities must
  change**.

## What does Entropy Pooling optimize?

Among all probability distributions that satisfy the views, Entropy
Pooling chooses the one that is closest to the prior distribution.

Closeness is measured using relative entropy, or the Kullback–Leibler
divergence:

$$D_{\mathrm{KL}}(q \| p) = \sum_{j=1}^{J} q_j \left[ \log(q_j)-\log(p_j) \right].$$

The full-confidence posterior is therefore

$$q^* = \arg\min_q D_{\mathrm{KL}}(q \| p)$$

subject to the constraints implied by the views.

In words:

> **Satisfy the views while introducing the smallest possible distortion
> relative to the prior probabilities.**

For a detailed explanation of the method, see [How does Entropy Pooling
work?](articles/how-does-EP-work.html).

## Confidence in the views

The posterior returned by Entropy Pooling represents **full confidence**
in the imposed views.

A user may instead want to retain part of the original prior model.

If

$$
c\in[0,1]
$$

denotes confidence in the views, the confidence-weighted probabilities
can be written as

$$
p_c = (1-c)p+cq^*.
$$

When

$$
c=0,
$$

the prior is recovered:

$$
p_c=p.
$$

When

$$
c=1,
$$

the full-confidence posterior is recovered:

$$
p_c=q^*.
$$

This separates two conceptually different questions:

1.  **What do I believe?**
2.  **How strongly do I believe it?**

The first question determines the view. The second determines the
confidence assigned to that view.

## Inspecting the posterior

Posterior probabilities should not be treated as a black box.

`ffp` provides tools for checking how the update affected the scenario
distribution.

### Moments

``` r
ffp_moments(x, posterior)
```

This is useful for verifying that mean, volatility, covariance, or
correlation views moved in the intended direction.

### Relative entropy

``` r
relative_entropy(prior, posterior)
```

This measures how far the posterior probabilities moved from the prior.

### Effective number of scenarios

``` r
ens(posterior)
```

The Effective Number of Scenarios helps identify whether the posterior
has concentrated too much probability on a small subset of observations.

### Distributional statistics

``` r
empirical_stats(
  x = x,
  p = posterior
)
```

This can be used to inspect changes in moments and tail-risk measures
under the new probability distribution.

A useful checklist is:

| Question                                          | Function             |
|:--------------------------------------------------|:---------------------|
| Did the moments move as intended?                 | `ffp_moments()`      |
| How far did the posterior move from the prior?    | `relative_entropy()` |
| Did probability become too concentrated?          | `ens()`              |
| What happened to the empirical risk distribution? | `empirical_stats()`  |
| What do the probabilities look like?              | `autoplot()`         |

## Using the posterior

Finding the posterior probabilities is usually not the end of the
analysis.

Once the probabilities have been updated, they can be carried into
downstream procedures.

### Risk analysis

``` r
empirical_stats(
  x = x,
  p = posterior
)
```

The historical scenarios are unchanged, but the statistics are now
computed under the posterior probabilities.

### Resampling scenarios

``` r
set.seed(123)

simulated <- bootstrap_scenarios(
  x = x,
  p = posterior,
  n = 10000L
)
```

This is useful when a downstream procedure expects a conventional
scenario sample rather than an explicit probability vector.

The complete workflow can therefore be summarized as

$$
\boxed{
\text{historical or simulated scenarios}
\rightarrow
\text{flexible probabilities}
\rightarrow
\text{conditioning or views}
\rightarrow
\text{posterior}
\rightarrow
\text{risk and portfolio analysis}
}.
$$

## Learn `ffp`

The documentation is organized around two complementary tutorials.

### Understand Entropy Pooling

If you want to understand the intuition, notation, and mathematical
structure behind the method, start with:

[**How does Entropy Pooling work?**](articles/how-does-EP-work.html)

This vignette explains:

- prior and posterior probabilities;
- how views become constraints;
- relative entropy;
- the dual optimization problem;
- confidence in the views.

### Use Entropy Pooling in practice

If you want to learn how the package works in R, continue with:

[**Entropy Pooling in practice**](articles/views.html)

This vignette covers:

- the basic `ffp` workflow;
- all `view_on_*()` constructors;
- `bind_views()`;
- solver selection;
- diagnostics;
- confidence;
- advanced distributional views;
- using the posterior in downstream analysis.

### Function reference

For individual functions and arguments, see the:

[**Function reference**](reference/index.html)

A useful path for a new user is therefore:

$$
\boxed{
\text{Quick Start}
\rightarrow
\text{How does EP work?}
\rightarrow
\text{EP in practice}
\rightarrow
\text{Reference}
}.
$$

## References

The package builds primarily on the Fully Flexible Probabilities and
Fully Flexible Views frameworks developed by Attilio Meucci.

Meucci, A. (2008). *Fully Flexible Views: Theory and Practice*. Risk,
21(10), 97–102.

Meucci, A. (2010). *Historical Scenarios with Fully Flexible
Probabilities*. GARP Risk Professional, 47–51.
