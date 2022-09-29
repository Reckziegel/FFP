## Release Summary

This is a patch release (from 0.2.1 to 0.2.2)

## Test environments
* local R installation, R 4.1.3
* ubuntu 16.04 (on travis-ci), R 4.1.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* Second submission for the version 0.2.2. I've corrected the (possibly) invalid URLs.

* I've used `\donttest{}` in `double_decay()` and `view_on_copula()` examples because they take more than 5 secs to run.
