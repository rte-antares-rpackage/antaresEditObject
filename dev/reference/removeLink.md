# Remove a link between two areas

![Antares API OK](figures/badge_api_ok.svg)

Remove a link between two areas in an Antares study.

## Usage

``` r
removeLink(from, to, opts = antaresRead::simOptions())
```

## Arguments

- from, to:

  The two areas linked together.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
createLink(from = "myarea", to  = "myarea2")
removeLink(from = "myarea", to  = "myarea2")
} # }
```
