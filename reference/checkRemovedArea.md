# Seek for a removed area

Check if it remains trace of a deleted area in the input folder

## Usage

``` r
checkRemovedArea(area, all_files = TRUE, opts = antaresRead::simOptions())
```

## Arguments

- area:

  An area

- all_files:

  Check files in study directory.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

a named list with two elements

## Examples

``` r
if (FALSE) { # \dontrun{
checkRemovedArea("myarea")
} # }
```
