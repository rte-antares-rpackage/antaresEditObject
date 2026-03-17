# Update input parameters of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Update input parameters of an Antares study

## Usage

``` r
updateInputSettings(import, opts = antaresRead::simOptions())
```

## Arguments

- import:

  Series to import.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{

updateInputSettings(import = c("thermal"))
updateInputSettings(import = c("hydro", "thermal"))

} # }
```
