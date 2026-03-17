# Activate RES in an Antares study

Helper to activate Renewables Energy Sources. This will update
`renewable.generation.modelling` parameter and create appropriate
structure for RES clusters.

## Usage

``` r
activateRES(opts = antaresRead::simOptions(), quietly = !interactive())
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- quietly:

  Display or not a message to the user if success.

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresEditObject)
tmp <- tempfile()
createStudy(path = tmp)
opts <- antaresRead::setSimulationPath(tmp)
activateRES()

# then you can use createClusterRES()...

} # }
```
