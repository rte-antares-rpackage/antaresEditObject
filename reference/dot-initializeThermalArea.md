# Initialize thermal data for a new area. For disk mode only.

Initialize thermal data for a new area. For disk mode only.

## Usage

``` r
.initializeThermalArea(name, overwrite, economic_options, opts)
```

## Arguments

- name:

  Name of the area as a character, without punctuation except - and \_.

- overwrite:

  Overwrite the area if already exists.

- economic_options:

  Economic options.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.
