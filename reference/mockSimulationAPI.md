# Mock API usage

Use this to generate command without an active API connection, it allow
to use function to edit a study to later on get API commands.

## Usage

``` r
mockSimulationAPI(force = FALSE, antares_version = "8.2.0")
```

## Arguments

- force:

  Logical, force mocking simulation even if
  [antaresRead::setSimulationPathAPI](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
  has already been called.

- antares_version:

  Antares version number.

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Mock simulation API
mockSimulationAPI()
# Create an area
createArea("new area")
# Get commands
getVariantCommands()
} # }
```
