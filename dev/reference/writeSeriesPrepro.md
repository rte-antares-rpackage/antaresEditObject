# Write prepro data

![Antares API NO](figures/badge_api_no.svg)

This function allows to write load, wind and solar prepro data. Using
`character(0)` allows to erase data (cf Examples).

## Usage

``` r
writeSeriesPrepro(
  area,
  type = c("load", "wind", "solar"),
  coefficients = NULL,
  daily_profile = NULL,
  translation = NULL,
  conversion = NULL,
  overwrite = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to write prepro data.

- type:

  Type of data to write : `"load"`, `"wind"` or `"solar"`.

- coefficients:

  A 12\*6 matrix of monthly values for the primary parameters alpha,
  beta, gamma, delta, theta and mu.

- daily_profile:

  A 24\*12 matrix of hourly / monthly coefficients K(hm) that are used
  to modulate the values of the stationary stochastic process by which
  the actual process is approximated.

- translation:

  A vector of length 8760 (or 8760\*1 matrix) to add to the time-series
  generated, prior or after scaling.

- conversion:

  A 2\*N matrix (with 1\<=N\<=50) that is used to turn the initial
  time-series produced by the generators into final data. See Antares
  General Reference Guide.

- overwrite:

  Logical. Overwrite the values if a file already exists.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Examples

``` r
if (FALSE) { # \dontrun{

writeSeriesPrepro("fictive_area", type = "solar", daily_profile = matrix(rep(1, 24*12), nrow = 24))

# Erase daily profile data:
writeSeriesPrepro("fictive_area", type = "solar", daily_profile = character(0))

} # }
```
