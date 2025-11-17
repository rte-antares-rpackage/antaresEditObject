# Create an area in an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Create a new area in an Antares study.

## Usage

``` r
createArea(
  name,
  color = grDevices::rgb(230, 108, 44, max = 255),
  localization = c(0, 0),
  nodalOptimization = nodalOptimizationOptions(),
  filtering = filteringOptions(),
  adequacy = adequacyOptions(),
  overwrite = FALSE,
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

- overwrite:

  Overwrite the area if already exist.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`editArea()`](editArea.md), [`removeArea()`](removeArea.md)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)

# Set simulation path
setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Create a new area
createArea("fictive_area")

} # }
```
