# Write water values

![Antares API OK](figures/badge_api_ok.svg)

Write water values for a given area.

## Usage

``` r
writeWaterValues(
  area,
  data = NULL,
  overwrite = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to add the water values.

- data:

  A 365x101 numeric matrix: table of marginal values for the stored
  energy, which depends on the date (365 days) and on the reservoir
  level (101 round percentage values ranging from 0% to 100%). OR a
  3-column matrix with 365x101 rows. In this latter case the 3 columns
  must be 'date', 'level' and 'value' (in this order), and the rows must
  be sorted by: ascending day, ascending level.

- overwrite:

  Logical. Overwrite the values if a file already exists.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Examples

``` r
if (FALSE) { # \dontrun{

writeWaterValues("fictive_area", data = matrix(rep(0, 365*101), nrow = 365))

} # }
```
