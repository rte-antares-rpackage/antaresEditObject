# Create a study's variant

**API**: create a new variant for a given study or use a pre-existing
one.

## Usage

``` r
createVariant(name, opts = antaresRead::simOptions())

useVariant(name, variant_id = NULL, opts = antaresRead::simOptions())
```

## Arguments

- name:

  Name for the variant to create or the name of an existent variant.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- variant_id:

  ID of the variant to use, if specified `name` is ignored.

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# See vignette for complete documentation
vignette("api-variant-management")
} # }
```
