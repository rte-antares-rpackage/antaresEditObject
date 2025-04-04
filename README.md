# antaresEditObject <img src="man/figures/antares_simulator.png" align="right" alt="" width=250 />
<br/>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/antaresEditObject)](https://CRAN.R-project.org/package=antaresEditObject)
[![cranlogs](https://cranlogs.r-pkg.org/badges/antaresEditObject)](https://cran.r-project.org/package=antaresEditObject)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/rte-antares-rpackage/antaresEditObject/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rte-antares-rpackage/antaresEditObject/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/rte-antares-rpackage/antaresEditObject/graph/badge.svg)](https://app.codecov.io/gh/rte-antares-rpackage/antaresEditObject)
<!-- badges: end -->

> Edit an Antares study before running a simulation.


## Installation

Install from CRAN:

```r
install.packages("antaresEditObject")
```

Or install dev version from GitHub:

```r
remotes::install_github("rte-antares-rpackage/antaresEditObject", build_vignettes = TRUE)
```


## Goal

This package allows to edit an Antares study. Methods are provided to create
(and remove) areas, links between them, thermal clusters and binding
constraints. These steps may be useful before running an Antares simulation.

See website for more documentation: https://rte-antares-rpackage.github.io/antaresEditObject/


## Usage

Usage of the package is detailed in the vignette: `vignette("antaresEditObject")`.

