# Activate st-storage in an Antares study

Activate st-storage in an Antares study

## Usage

``` r
activateST(opts = antaresRead::simOptions(), quietly = !interactive())
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

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
activateST()

# then you can use createClusterST()...

} # }
```
