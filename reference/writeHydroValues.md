# Write Hydro Values

![Antares API OK](figures/badge_api_ok.svg)

Write waterValues, reservoirLevels, maxpower, inflowPattern and
creditModulations data for a given area.

## Usage

``` r
writeHydroValues(
  area,
  type,
  data,
  overwrite = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to add the values.

- type:

  Type of hydro file, it can be "waterValues", "reservoir", "maxpower",
  "inflowPattern" or "creditmodulations".

- data:

  The data must have specific dimension depending on the type of file :

  - `waterValues`: a 365x101 numeric matrix: marginal values for the
    stored energy based on date (365 days) and on the reservoir level
    (101 round percentage values ranging from 0% to 100%). OR a 3-column
    matrix with 365x101 rows. In this latter case the 3 columns must be
    'date', 'level' and 'value' (in this order), and the rows must be
    sorted by: ascending day, ascending level.

  - `reservoir`: a 365x3 numeric matrix. The columns contains
    respectively the levels min, avg and max.

  - `maxpower`: a 365x4 numeric matrix.

  - `inflowPattern`: a 365x1 numeric matrix.

  - `creditmodulations`: a 2x101 numeric matrix.

- overwrite:

  Logical. Overwrite the values if a file already exists.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Warning

For an **Antares version \>= 860**, control of data consistency between
`mingen.txt` and `maxpower_<area>.txt` can be executed.

This control depends on the values you find in `hydro.ini` file.

## Examples

``` r
if (FALSE) { # \dontrun{

writeHydroValues("fictive_area", type = "inflowPattern", data = matrix(rep(0, 365*1), nrow = 365))

} # }
```
