# Check dimension of time series for binding constraints

Only needed for study version \>= 870

Dimension of groups are compared with meta parameter `binding` returned
by
[`antaresRead::simOptions()`](https://rte-antares-rpackage.github.io/antaresRead/reference/simOptions.html)

## Usage

``` r
group_values_meta_check(
  group_value,
  values_data,
  operator_check,
  output_operator,
  opts = antaresRead::simOptions()
)
```

## Arguments

- group_value:

  `character` name of group

- values_data:

  `list` values used by the constraint

- operator_check:

  `character` parameter "operator"

- output_operator:

  `character` for

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

NULL if it's new group to add or error exceptions with dimension control

An updated list containing various information about the simulation.
