# Initialize links data for a new area. For disk mode only.

Initialize links data for a new area. For disk mode only.

## Usage

``` r
.initializeLinksArea(name, overwrite, opts)
```

## Arguments

- name:

  Name of the area as a character, without punctuation except - and \_.

- overwrite:

  Overwrite the area if already exists.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.
