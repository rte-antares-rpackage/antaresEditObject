# Check if mingen data and maxpower data are consistent

At each hourly time step, mingen must be less or equal than
generatingMaxPower.

## Usage

``` r
check_mingen_vs_maxpower(area, opts = antaresRead::simOptions())
```

## Arguments

- area:

  The area where to check the data.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Value

a list containing the boolean if the check is ok and the message to
display.

## Note

Function called only for an **Antares version \>= 860**.
