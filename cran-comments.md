## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is an upgrade (version 0.1.0 to 0.2.0).

* Used `\donttest{}` in `double_decay()` and `view_on_copula()` examples because they take more than 5 secs to run.
