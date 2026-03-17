# Set API mode

Two modes are available when using the API:

- **async**: record all API calls, but nothing is sent to the server

- **sync**: send query to the API each time a function is used

## Usage

``` r
setAPImode(mode = c("sync", "async"), opts = antaresRead::simOptions())
```

## Arguments

- mode:

  The mode you want to use.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# See vignette for complete documentation
vignette("api-variant-management")

# Usage :
setAPImode("sync")

} # }
```
