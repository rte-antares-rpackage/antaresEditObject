# Remove an area from an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Remove an area in an Antares study.

## Usage

``` r
removeArea(name, opts = antaresRead::simOptions())
```

## Arguments

- name:

  An area name.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`createArea()`](createArea.md), [`editArea()`](editArea.md)

## Examples

``` r
if (FALSE) { # \dontrun{
removeArea("fictive_area")
} # }
```
