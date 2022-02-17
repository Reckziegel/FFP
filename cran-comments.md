## Release Summary

This is a minor release (from 0.1.0 to 0.2.0)

## Test environments
* local R installation, R 4.1.2
* ubuntu 16.04 (on travis-ci), R 4.1.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* I've used `\donttest{}` in `double_decay()` and `view_on_copula()` examples because they take more than 5 secs to run.

* This is the second time I'm submitting the package to CRAN this week. I have fixed a remaining 
  note by passing the `xts` package to suggests.
