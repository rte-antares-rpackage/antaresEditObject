# Detect a pattern in a binding constraint coefficient

Detect a pattern in a binding constraint coefficient

## Usage

``` r
detect_pattern_in_binding_constraint(pattern, opts = antaresRead::simOptions())
```

## Arguments

- pattern:

  The pattern to detect.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

the names of the binding constraints containing the pattern
