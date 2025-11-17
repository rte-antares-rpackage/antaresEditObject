# Edit an area in an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Edit an existing area in an Antares study.

## Usage

``` r
editArea(
  name,
  color = NULL,
  localization = NULL,
  nodalOptimization = NULL,
  filtering = NULL,
  adequacy = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- name:

  Name of the area as a character, without punctuation except - and \_.

- color:

  Color of the node

- localization:

  Localization on the map

- nodalOptimization:

  Nodal optimization parameters, see
  [`nodalOptimizationOptions()`](nodalOptimizationOptions.md).

- filtering:

  Filtering parameters, see [`filteringOptions()`](filteringOptions.md).

- adequacy:

  Adequacy parameters, see [`adequacyOptions()`](adequacyOptions.md).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`createArea()`](createArea.md), [`removeArea()`](removeArea.md)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)

# Set simulation path
setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Edit an existing area
editArea("area", color = grDevices::rgb(230, 108, 44, max = 255),
  localization = c(1, 1),
  opts = antaresRead::simOptions()) 

editArea("de",  nodalOptimization = list("spilledenergycost" = list(fr = 30)),
opts = antaresRead::simOptions())

editArea("de",  nodalOptimization = nodalOptimizationOptions(),
opts = antaresRead::simOptions())

editArea(
  "de",
  filtering = list("filter_synthesis"=paste(c("hourly","daily"),collapse = ", "))
)

} # }
```
