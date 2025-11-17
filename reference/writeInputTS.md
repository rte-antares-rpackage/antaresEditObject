# Write input time series

![Antares API OK](figures/badge_api_ok.svg)

This function writes input time series in an Antares project.

## Usage

``` r
writeInputTS(
  data,
  type = c("load", "hydroROR", "hydroSTOR", "mingen", "wind", "solar", "tsLink"),
  area = NULL,
  link = NULL,
  overwrite = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- data:

  A 8760\*N matrix of hourly time series, except when `type` is
  `"hydroSTOR"`. In this latter case `"hydroSTOR"` data must have either
  be 365 rows (Antares v7) or 12 rows (v6 and earlier).

- type:

  Serie to write: `"load"`, `"hydroROR"`, `"hydroSTOR"`, `"wind"`,
  `"solar"`, `"tsLink"` or `"mingen"`.

  If type == `"mingen"`, `"antaresVersion"` should be \>= 860. Refers to
  note section below.

- area:

  The area where to write the input time series.

- link:

  Link for which writing transmission capacities time series, must like
  `"area01%area02"` or `"area01 - area02"` or `c("area01", "area02")`.

- overwrite:

  Logical. Overwrite the values if a file already exists.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

For an **Antares version \>= 860**, the `mingen.txt` file is created.

The `mingen.txt` file can be created under two conditions:

- The number of columns must be equal to either `1` or the number in
  `mod.txt`

- If the `mod.txt` file is empty or has one column, then there is no
  dimension constraint

## Warning

You cannot use `area` and `link` arguments at the same time.

For an **Antares version \>= 860**, control of data consistency between
`mingen.txt` and `mod.txt` can be executed.

These controls depend on the values you find in `hydro.ini` file.

## Examples

``` r
if (FALSE) { # \dontrun{

# Write solar time series
writeInputTS(
  area = "fictive_area",
  type = "solar",
  data = matrix(rep(4, 8760*2), nrow = 8760)
)

} # }
```
