# Edit hydro.ini values

![Antares API OK](figures/badge_api_ok.svg)

For a given area, write its data in the hydro.ini file.

## Usage

``` r
writeIniHydro(area, params, mode = "other", opts = antaresRead::simOptions())
```

## Arguments

- area:

  The area where to edit the values.

- params:

  The list data must have specific names and specific types :

  - `follow load` : logical or NULL

  - `use heuristic` : logical or NULL

  - `use water` : logical or NULL

  - `hard bounds` : logical or NULL

  - `use leeway` : logical or NULL

  - `power to level` : logical or NULL

  - `reservoir` : logical or NULL

  - `inter-daily-breakdown` : numeric, integer or NULL

  - `intra-daily-modulation` : numeric, integer or NULL

  - `inter-monthly-breakdown` : numeric, integer or NULL

  - `leeway low` : numeric, integer or NULL

  - `leeway up` : numeric, integer or NULL

  - `pumping efficiency` : numeric, integer or NULL

  - `initialize reservoir date` : numeric, integer or NULL

  - `reservoir capacity` : numeric, integer or NULL

- mode:

  Execution mode. Useful when you create a new area or remove an
  existing area to avoid control on hydro data.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Warning

For an **Antares version \>= 860**, control of data consistency between
`mingen.txt` and `mod.txt` can be executed.

For an **Antares version \>= 860**, control of data consistency between
`mingen.txt` and `maxpower_<area>.txt` can be executed.

These controls depend on the values you find in `hydro.ini` file.

## Examples

``` r
if (FALSE) { # \dontrun{
opts <- setSimulationPath(studypath, simulation = "input")
createArea("fictive_area") 
writeIniHydro(area = "fictive_area"
, params = list("leeway low" = 2.5, "leeway up" = 25))

} # }
```
