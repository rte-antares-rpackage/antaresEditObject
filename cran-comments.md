## Test environments
* local R installation, R 4.1.1
* Ubuntu 20.04, Windows 10, macOS (on GitHub Actions), R 4.1.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

New release, no revdep, all OK (https://cran.r-project.org/web/checks/check_results_antaresEditObject.html)
Thanks!


## Fix CRAN NOTE for release 0.6.0
"Running R code in ‘testthat.R’ had CPU time 8.1 times elapsed time"

## Fix CRAN CHECKS on version 0.6.0 
* Fix ERROR on `r-devel-linux-x86_64-debian-gcc` cause function `base::NCOL()` is updated
