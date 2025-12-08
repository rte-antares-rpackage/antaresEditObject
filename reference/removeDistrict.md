# Remove a district

Remove a district

## Usage

``` r
removeDistrict(name, opts = simOptions())
```

## Arguments

- name:

  District's name.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`createDistrict()`](createDistrict.md),
[`editDistrict()`](editDistrict.md)

## Examples

``` r
if (FALSE) { # \dontrun{
removeDistrict(
  name = "mydistrict",
  opts = simOptions()
)
} # }
```
