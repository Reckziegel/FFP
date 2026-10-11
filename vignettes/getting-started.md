---
title: "Getting Started with Fully Flexible Probabilities"
author: "Bernardo Reckziegel"
date: "2026-09-03"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 2
vignette: >
  %\VignetteIndexEntry{Getting Started with Fully Flexible Probabilities}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette introduces **Fully Flexible Probabilities** (FFP) and the `ffp` package. It is designed for users new to the concepts and methods. If you want a practical walkthrough of entropy pooling workflows, see the vignette **Entropy Pooling in practice**.

## Why Fully Flexible Probabilities?

Imagine you work in finance and face this common situation: You have historical market data, but today you believe the market will behave differently than it did in the past. Perhaps you expect:

- **Higher returns** in one asset class
- **Lower volatility** due to changing market conditions
- **Different correlation** patterns between assets
- **Stronger dependence** in downturns

Your historical data still contains valuable information about the relationships between variables, but you want to **reweight** the past scenarios to reflect your forward-looking views.

This is what Fully Flexible Probabilities do: they let you adjust the probability of historical scenarios without discarding those scenarios. The framework preserves the joint structure of the data while incorporating your beliefs about the future.

## The Core Idea

The fundamental insight is simple:

> **Assign new probabilities to historical scenarios to match your views, while changing the probability distribution as little as possible.**

Let's denote:

- **$X$**: The matrix of historical scenarios (rows = scenarios, columns = variables)
- **$p$**: The original (prior) probability for each scenario
- **$q$**: The new (posterior) probability for each scenario, adjusted to match your views

The `ffp` package solves this mathematically using entropy pooling, an optimization approach that finds $q$ such that:

1. Your views are satisfied
2. The new probabilities are as "close" to the original ones as possible (in relative-entropy terms)
3. The resulting probabilities are valid (all non-negative, sum to 1)

## A First Example

Let's work with R's built-in `EuStockMarkets` dataset, which contains prices for four European stock indices.


``` r
library(ffp)
library(ggplot2)

# Convert prices to log-returns
x <- diff(log(EuStockMarkets))

# Prior probabilities: equal weight to all scenarios
prior <- rep(1 / nrow(x), nrow(x))

head(x)
#> Time Series:
#> Start = c(1991, 131) 
#> End = c(1991, 136) 
#> Frequency = 260 
#>                   DAX          SMI          CAC         FTSE
#> 1991.500 -0.009326550  0.006178360 -0.012658756  0.006770286
#> 1991.504 -0.004422175 -0.005880448 -0.018740638 -0.004889587
#> 1991.508  0.009003794  0.003271184 -0.005779182  0.009027020
#> 1991.512 -0.001778217  0.001483372  0.008743353  0.005771847
#> 1991.515 -0.004676712 -0.008933417 -0.005120160 -0.007230164
#> 1991.519  0.012427042  0.006737244  0.011714353  0.008517217
dim(x)
#> [1] 1859    4
```

Suppose you believe that the **FTSE will outperform its historical average by 20%**. How do you express this and find adjusted probabilities?

### Step 1: Define the View

Use `view_on_mean()` to express your belief about expected return:


``` r
target_ftse_mean <- mean(x[, "FTSE"]) * 1.20

view <- view_on_mean(
  x = as.matrix(x[, "FTSE"]),
  mean = target_ftse_mean
)

view
#> # ffp view <Mean>
#>   1 equality constraint
#>   Scenarios: 1859
#> 
#>   Aeq  1 × 1859
#>   beq  1 × 1
```

The `view` object contains the mathematical representation of your belief (matrices `Aeq` and `beq`) that the optimizer will use.

### Step 2: Solve for Posterior Probabilities

Pass the view to `entropy_pooling()`:


``` r
posterior <- entropy_pooling(
  p = prior,
  Aeq = view$Aeq,
  beq = view$beq,
  solver = "nlminb"
)

posterior
#> <ffp[1859]>
#> 0.0005425631 0.0005340012 0.000544236 0.0005418246 0.0005322989 ... 0.0005451271
```

The result is an `ffp` object: a vector of posterior probabilities, one for each historical scenario.

### Step 3: Verify the Result

Check that your view is satisfied:


``` r
# Prior FTSE mean
cat("Prior FTSE mean:      ", mean(x[, "FTSE"]), "\n")
#> Prior FTSE mean:       0.0004319851

# Posterior FTSE mean (weighted by posterior probabilities)
cat("Posterior FTSE mean:  ", ffp_moments(x, posterior)$mu["FTSE"], "\n")
#> Posterior FTSE mean:   0.0005183822

# Target
cat("Target FTSE mean:     ", target_ftse_mean, "\n")
#> Target FTSE mean:      0.0005183821
```

The posterior mean matches the target. The optimization successfully reweighted the historical scenarios to reflect your view.

### Step 4: Visualize the Probabilities


``` r
autoplot(posterior)
```

<div class="figure" style="text-align: center">
<img src="figure/plot-posterior-1.png" alt="plot of chunk plot-posterior"  />
<p class="caption">plot of chunk plot-posterior</p>
</div>

The plot shows how the posterior probabilities are distributed across the historical scenarios. Notice:

- **Color gradient**: Blue represents low probability, red represents high probability
- **Peak marker**: The dashed line marks the scenario with maximum probability
- **Statistics**: The subtitle reports mean, median, and standard deviation

## What Is an `ffp` Object?

An `ffp` is an R S3 class built on top of numeric vectors. It represents a **single probability distribution** for a set of scenarios.


``` r
# It's a vector of probabilities
class(posterior)
#> [1] "ffp"        "vctrs_vctr"
length(posterior)
#> [1] 1859
sum(posterior)  # Always sums to 1
#> [1] 1

# Extract as numeric if needed
head(vctrs::vec_data(posterior))
#> [1] 0.0005425631 0.0005340012 0.0005442360 0.0005418246 0.0005322989
#> [6] 0.0005438576
```

Key properties:

- **Always valid**: Sums to 1, all values non-negative
- **Type-safe**: Arithmetic operations are restricted to prevent accidental misuse
- **Works with ggplot2**: The `autoplot()` method provides publication-ready visualization

## Common Workflows

The `ffp` package supports different types of views. Here are the most common:

### View on Expected Return (Means)


``` r
# I believe the CAC will have an expected return of 0.001 (0.1%)
view <- view_on_mean(
  x = as.matrix(x[, "CAC"]),
  mean = 0.001
)

posterior <- entropy_pooling(p = prior, Aeq = view$Aeq, beq = view$beq)
```

### View on Volatility


``` r
# I believe CAC volatility will be 15% lower than historical
view <- view_on_volatility(
  x = as.matrix(x[, "CAC"]),
  vol = sd(x[, "CAC"]) * 0.85
)

posterior <- entropy_pooling(p = prior, Aeq = view$Aeq, beq = view$beq)
```

### View on Relative Performance (Ranking)


``` r
# I believe DAX will outperform SMI
view <- view_on_rank(
  x = x[, c("DAX", "SMI")],
  rank = c(2, 1)  # SMI is worst (1), DAX is best (2)
)

posterior <- entropy_pooling(p = prior, A = view$A, b = view$b, solver = "nloptr")
```

Notice that ranking produces **inequality** constraints (`A`, `b`), not equality constraints. This requires a different solver.

### View on Correlation


``` r
# Increase correlation between FTSE and DAX to 0.90
target_cor <- cor(x)
target_cor["FTSE", "DAX"] <- 0.90
target_cor["DAX", "FTSE"] <- 0.90

view <- view_on_correlation(x = x, cor = target_cor)

posterior <- entropy_pooling(p = prior, Aeq = view$Aeq, beq = view$beq)
```

## Combining Multiple Views

Real applications usually involve **more than one view**. Use `bind_views()` to combine them:


``` r
# View 1: FTSE mean 20% above historical
view_ftse_mean <- view_on_mean(
  x = as.matrix(x[, "FTSE"]),
  mean = mean(x[, "FTSE"]) * 1.20
)

# View 2: CAC volatility 10% below historical
view_cac_vol <- view_on_volatility(
  x = as.matrix(x[, "CAC"]),
  vol = sd(x[, "CAC"]) * 0.90
)

# Combine them
views <- bind_views(view_ftse_mean, view_cac_vol)

# Solve
posterior_combined <- entropy_pooling(
  p = prior,
  Aeq = views$Aeq,
  beq = views$beq,
  solver = "nlminb"
)
```

Verify both views are satisfied:


``` r
moments <- ffp_moments(x, posterior_combined)

cat("FTSE mean:\n")
#> FTSE mean:
cat("  Prior:     ", mean(x[, "FTSE"]), "\n")
#>   Prior:      0.0004319851
cat("  Posterior: ", moments$mu["FTSE"], "\n")
#>   Posterior:  0.0005183821
cat("  Target:    ", mean(x[, "FTSE"]) * 1.20, "\n\n")
#>   Target:     0.0005183821

cat("CAC volatility:\n")
#> CAC volatility:
cat("  Prior:     ", sd(x[, "CAC"]), "\n")
#>   Prior:      0.01103088
cat("  Posterior: ", sqrt(moments$sigma["CAC", "CAC"]), "\n")
#>   Posterior:  0.009925293
cat("  Target:    ", sd(x[, "CAC"]) * 0.90, "\n")
#>   Target:     0.009927788
```

## Diagnostics: Did It Work?

After solving entropy pooling, always check the result. The package provides several tools:

### 1. Moments Check

Did the views actually change the right moments?


``` r
ffp_moments(x, posterior_combined)
#> $mu
#>          DAX          SMI          CAC         FTSE 
#> 0.0007627039 0.0009316216 0.0004904584 0.0005183821 
#> 
#> $sigma
#>               DAX          SMI          CAC         FTSE
#> DAX  8.891996e-05 5.343667e-05 6.556804e-05 4.331768e-05
#> SMI  5.343667e-05 7.429211e-05 4.872649e-05 3.589152e-05
#> CAC  6.556804e-05 4.872649e-05 9.851143e-05 4.628255e-05
#> FTSE 4.331768e-05 3.589152e-05 4.628255e-05 5.776979e-05
```

### 2. Relative Entropy

How much did the probabilities move away from the prior? Larger values mean bigger changes.


``` r
relative_entropy(prior = prior, posterior = vctrs::vec_data(posterior_combined))
#> [1] 0.005584913
```

### 3. Effective Number of Scenarios

Did probability become too concentrated? This warns if few scenarios get very high weight.


``` r
c(
  prior = ens(as_ffp(prior)),
  posterior = ens(posterior_combined)
)
#>     prior posterior 
#>  1859.000  1848.647
```

Values close to the number of scenarios are good. Sharp drops suggest problematic concentration.

### 4. Empirical Statistics

What happened to the full empirical distribution (mean, std, skewness, kurtosis, VaR, CVaR)?


``` r
empirical_stats(x, posterior_combined)
#> # A tibble: 24 × 3
#>    stat  name      value
#>    <fct> <chr>     <dbl>
#>  1 Mu    DAX    0.000763
#>  2 Mu    SMI    0.000932
#>  3 Mu    CAC    0.000490
#>  4 Mu    FTSE   0.000518
#>  5 Std   DAX    0.00944 
#>  6 Std   SMI    0.00863 
#>  7 Std   CAC    0.00993 
#>  8 Std   FTSE   0.00760 
#>  9 Skew  DAX   -0.0762  
#> 10 Skew  SMI   -0.132   
#> # ℹ 14 more rows
```

## Partial Confidence in Views

Sometimes you don't have full confidence in your views. The framework lets you express this by **blending the posterior with the prior**:

$$p_{\text{blended}} = (1 - c) \times p + c \times q$$

where $c \in [0, 1]$ is your confidence level.


``` r
confidence <- 0.60

posterior_blended <- as_ffp(
  (1 - confidence) * prior + 
    confidence * vctrs::vec_data(posterior_combined)
)

# Check: at confidence=0, we get the prior; at confidence=1, we get posterior
cat("Prior FTSE mean:     ", mean(x[, "FTSE"]), "\n")
#> Prior FTSE mean:      0.0004319851
cat("Blended FTSE mean:   ", ffp_moments(x, posterior_blended)$mu["FTSE"], "\n")
#> Blended FTSE mean:    0.0004838233
cat("Posterior FTSE mean: ", ffp_moments(x, posterior_combined)$mu["FTSE"], "\n")
#> Posterior FTSE mean:  0.0005183821
```

The blended distribution lies between the prior and posterior, reflecting your actual confidence level.

## From Probabilities to Decisions

Once you have posterior probabilities, you can use them throughout your analysis:

### Compute Risk Statistics


``` r
empirical_stats(x, posterior_combined)
#> # A tibble: 24 × 3
#>    stat  name      value
#>    <fct> <chr>     <dbl>
#>  1 Mu    DAX    0.000763
#>  2 Mu    SMI    0.000932
#>  3 Mu    CAC    0.000490
#>  4 Mu    FTSE   0.000518
#>  5 Std   DAX    0.00944 
#>  6 Std   SMI    0.00863 
#>  7 Std   CAC    0.00993 
#>  8 Std   FTSE   0.00760 
#>  9 Skew  DAX   -0.0762  
#> 10 Skew  SMI   -0.132   
#> # ℹ 14 more rows
```

### Resample Scenarios

Generate new scenario samples using the posterior distribution:


``` r
set.seed(42)

resampled <- bootstrap_scenarios(
  x = x,
  p = posterior_combined,
  n = 5000L
)

head(resampled)
#> # A tibble: 6 × 4
#>         DAX      SMI       CAC     FTSE
#>       <dbl>    <dbl>     <dbl>    <dbl>
#> 1  0.0326    0        0.0132    0.0112 
#> 2  0.0185    0.0166   0.00722   0.00920
#> 3 -0.00123   0.00389 -0.000502  0.00219
#> 4  0         0        0        -0.0104 
#> 5  0.000577 -0.00955 -0.0104   -0.00513
#> 6  0.00983   0.00252  0.00922   0.0114
```

This is useful when downstream applications expect scenario samples rather than explicit probability vectors.

## When Should You Use FFP?

Fully Flexible Probabilities are useful for:

✓ **Stress testing**: Adjust historical scenarios to reflect stressed conditions  
✓ **Forward-looking views**: Incorporate beliefs about future market behavior  
✓ **Portfolio construction**: Optimize portfolios under your views  
✓ **Risk aggregation**: Combine multiple perspectives on market evolution  
✓ **Scenario analysis**: Move probabilities while preserving historical joint dependence  
✓ **Non-normal distributions**: Work with empirical scenarios, not just parameter estimates  

They are **not** ideal for:

✗ Replacing historical data with parametric models (when you have enough data)  
✗ Situations requiring only small probabilistic adjustments (standard Bayesian updating may suffice)  
✗ Applications where altering scenario probabilities is conceptually wrong  

## Key Takeaways

1. **FFP reweights scenarios**: You keep historical scenarios but change their probabilities
2. **Views are flexible**: Express beliefs on means, volatility, correlation, ranking, and more
3. **Entropy pooling optimizes**: Finds probabilities that satisfy views while minimizing distortion
4. **Always diagnose**: Check that views are satisfied and probabilities don't concentrate too much
5. **Blend when uncertain**: Use partial confidence to express less-than-certain views
6. **Use the output**: Apply posterior probabilities in portfolio construction, risk measurement, and resampling

## Next Steps

- **Entropy Pooling in Practice** vignette: Detailed workflow examples and all view types
- **How does Entropy Pooling Work?** vignette: Mathematical foundations and theory
- **Views** vignette: Deep dive into each view constructor with advanced examples
- Function reference: `?ffp`, `?entropy_pooling`, `?view_on_mean`, and related functions

## References

Meucci, A. (2008). *Fully Flexible Views: Theory and Practice*. Risk, 21(10), 97–102.

For theoretical background and mathematical details, see the companion vignettes and Meucci's original paper.
