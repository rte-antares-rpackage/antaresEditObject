# Rollback to previous hydro data if the data is not consistent

Rollback the data to previous one if the check is KO. For a given area,
check if the data is consistent and rollback to previous data if the
check is KO.

## Usage

``` r
rollback_to_previous_data(
  area,
  prev_data,
  rollback_type,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to execute the control and rollback the data.

- prev_data:

  The original data to restore if necessary.

- rollback_type:

  The file to restore and the control(s) to execute.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

## Note

Function called only for an **Antares version \>= 860**.
