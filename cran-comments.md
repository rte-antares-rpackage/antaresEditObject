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

## Fix TESTS (in this patch version 0.6.2) to remove rev dep to package `antaresRead` 
* we have issue to next release of package `antaresRead` (v2.6.1), see below

Changes to worse in reverse depends:

Package: antaresEditObject
Check: tests
New result: ERROR
    Running ‘testthat.R’ [54s/51s]
    
Failure ('test-createCluster.R:76:5'): Remove all clusters ──────────────────
`antaresRead::readClusterDesc()` did not throw an error.

## antaresEditObject 0.6.4 
Patch version to break abusive dependencies with antaresRead in tests

## antaresEditObject 0.7.1

### Re Submission  

 - Check: PDF version of manual, Result: WARNING
  LaTeX errors when creating PDF version.
  This typically indicates Rd problems.
  LaTeX errors found:
  ! LaTeX Error: Unicode character (U+26A0)
                 not set up for use with LaTeX.
                 
Fix local error (R CMD check antaresEditObject_0.7.1.tar.gz) :  

 - When sourcing 'api-variant-management.R':
Erreur : Cannot find the file(s): "../man/figures/badge_api_ok.svg"  


This error did not appear on the CRAN

### Re Submission 2
  
  - Fix TRUE and FALSE instead of T and F  
  - `\dontrun{}` sections are needed because most of functions need to be apply on a study load on Global env (legacy code too)
  
