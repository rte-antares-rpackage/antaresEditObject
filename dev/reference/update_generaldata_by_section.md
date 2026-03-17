# Update a specific section in generaldata.ini file

Update a specific section in generaldata.ini file

## Usage

``` r
update_generaldata_by_section(opts, section, new_params)
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- section:

  The section to update.

- new_params:

  The values to write in the section.

## Value

An updated list containing various information about the simulation.
