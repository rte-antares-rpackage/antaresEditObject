# Write default input time series if `mingen.txt` or/and `mod.txt` is empty

Write default input time series if `mingen.txt` or/and `mod.txt` is
empty

## Usage

``` r
fill_empty_hydro_ts_file(area, opts = antaresRead::simOptions())
```

## Arguments

- area:

  The area where to write the input time series.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).
