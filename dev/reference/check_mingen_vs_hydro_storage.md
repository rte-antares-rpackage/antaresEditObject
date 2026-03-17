# Check if mingen data and hydro storage data are consistent

At each weekly/monthly/annual time step, mingen must be less or equal
than hydro storage.

## Usage

``` r
check_mingen_vs_hydro_storage(area, opts = antaresRead::simOptions())
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
