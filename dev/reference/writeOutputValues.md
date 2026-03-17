# Write output value for Antares

![Antares API NO](figures/badge_api_no.svg)

This function write all output values for an Antares study.

## Usage

``` r
writeOutputValues(data, opts = NULL)
```

## Arguments

- data:

  obtain with readAntares

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)
library(data.table)
opts <- setSimulationPath("my_study")
data <- readAntares(links = "all", areas = "all", clusters = "all")
writeOutputValues(data)

} # }
```
