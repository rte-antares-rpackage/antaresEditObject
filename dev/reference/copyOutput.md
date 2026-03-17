# Copy of the output files of an Antares study

![Antares API NO](figures/badge_api_no.svg)

Copy of the output files of an Antares study.

## Usage

``` r
copyOutput(opts, extname, mcYears = "all")
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- extname:

  Extension to be added to the name of the study, to be used as a name
  for the newly created folder.

- mcYears:

  mcYears to copy. Can be `"all"`.

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)

# Set simulation path
opts = setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Create a new area
copyOutput(opts, "_adq")

} # }
```
