# Create a district

Allows selecting a set of areas so as to bundle them together in a
"district".

## Usage

``` r
createDistrict(
  name,
  caption = NULL,
  comments = NULL,
  apply_filter = c("none", "add-all", "remove-all"),
  add_area = NULL,
  remove_area = NULL,
  output = FALSE,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- name:

  District's name.

- caption:

  Caption for the district.

- comments:

  Comments for the district.

- apply_filter:

  Possible values are `add-all` to add all areas to the district,
  `remove-all` to clear the district, or `none` (default) to don't apply
  a filter.

- add_area:

  Character vector of area(s) to add to the district.

- remove_area:

  Character vector of area(s) to remove from the district.

- output:

  Logical, compute the results for the district or not?

- overwrite:

  Logical, should the district be overwritten if already exist?

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
createDistrict(
  name = "mydistrict",
  apply_filter = "add-all",
  remove_area = c("fr", "be")
)
} # }
```
