# Edit area's ui in API mode.

Edit area's ui in API mode.

## Usage

``` r
.api_command_execute_edit_area_ui(name, color, localization, opts)
```

## Arguments

- name:

  Name of the area as a character, without punctuation except - and \_.

- color:

  Color of the node

- localization:

  Localization on the map

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
