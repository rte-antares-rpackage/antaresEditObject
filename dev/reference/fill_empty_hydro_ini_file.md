# Write default values in hydro.ini file if the section is empty

For a given area, if the data is empty, pick value from default values
for `use heuristic`, `follow load` and `reservoir` sections.

## Usage

``` r
fill_empty_hydro_ini_file(area, opts = antaresRead::simOptions())
```

## Arguments

- area:

  The area where to write the value, i.e. lhs in the section.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).
