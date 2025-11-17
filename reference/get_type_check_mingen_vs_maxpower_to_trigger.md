# Get the type of control to execute between mingen data and maxpower data

Compute the type of control to make between :

- `input/hydro/series/<area>/mingen.txt`

- `input/hydro/common/capacity/maxpower_<area>.txt`

This control is implemented in Antares too. No control to execute if
`reservoir` section in hydro.ini for the area is set to TRUE.

## Usage

``` r
get_type_check_mingen_vs_maxpower_to_trigger(
  area,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where the type of control must be computed.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Value

a character containing the type of control to execute.

## Note

Function called only for an **Antares version \>= 860**.
