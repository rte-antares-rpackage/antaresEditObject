# Update output parameters of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Update output parameters of an Antares study

## Usage

``` r
updateOutputSettings(
  synthesis = NULL,
  storenewset = NULL,
  archives = NULL,
  result.format = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- synthesis:

  Logical. If TRUE, synthetic results will be stored in a directory
  Study_name/OUTPUT/simu_tag/Economy/mc-all. If FALSE, No general
  synthesis will be printed out. See Antares General Reference Guide
  (see link below).

- storenewset:

  Logical. See Antares General Reference Guide (see link below).

- archives:

  Character vector. Series to archive. See Antares General Reference
  Guide (see link below).

- result.format:

  Character. Output format (txt-files or zip). See Antares General
  Reference Guide (see link below).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

Antares General Reference Guide

## Examples

``` r
if (FALSE) { # \dontrun{

updateOutputSettings(
  synthesis = TRUE,
  storenewset = FALSE,
  archives = c("load", "wind"),
  result.format = "zip"
)

} # }
```
