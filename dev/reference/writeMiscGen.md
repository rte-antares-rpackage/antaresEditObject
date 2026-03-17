# Write Misc Gen data

![Antares API OK](figures/badge_api_ok.svg)

## Usage

``` r
writeMiscGen(data, area, opts = antaresRead::simOptions())
```

## Arguments

- data:

  Data to write.

- area:

  Name of the area for which to write data.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{

writeMiscGen(matrix(data = c(rep(0, 8760 * 7), rep(-100000, 8760)), ncol = 8), "area1")

} # }
```
