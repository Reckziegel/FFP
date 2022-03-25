# ffp 0.2.1

* Bug fix in `entropy_pooling()` when dealing with multiple inequalities constraints;
* Small changes in the documentation files.

# ffp 0.2.0

## New Features

* A new class (`ffp_views`) was added to help the user to input views on the market for portfolio construction and simulation purposes. See the `view_*()` family of functions;
* `entropy_pooling()` is now exported and can be used with three different solvers (`nlminb`, `solnl`, and `nloptr`);
* `bind_views()` allows the user to "glue" multiple views into the same object;
* `ffp_moments()` computes the location and dispersion parameters under flexible probabilities. 

## Minor improvements and fixes

* `bind_probs()` now returns the user call in the third column instead the old arbitrary `key` column (#13);

# ffp (development version)

# ffp 0.1.0

* CRAN release.

# ffp 0.0.1

* The package now is built upon S3 `vctrs`, instead of `tibbles`;
* Functions `bootstrap_scenarios()`, `scenario_density()` and `bind_probs()` only accepts objects of `ffp` class;
* Added `ffp` method to `ggplot2::autoplot()`;
* Exponential Smoothing is now computed with `exp_decay()`. 

# ffp 0.0.9000

* First release of the development version of `ffp`.
