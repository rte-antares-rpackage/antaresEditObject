# Edit area's parameters in API mode.

Edit area's parameters in API mode.

## Usage

``` r
.api_command_execute_edit_area(name, new_values, type, opts)
```

## Arguments

- name:

  Name of the area to edit.

- new_values:

  Values of the parameters to edit.

- type:

  Type of edition.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.
