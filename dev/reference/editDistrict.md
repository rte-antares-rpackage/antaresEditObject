# Edit a district in an Antares study

Edit a district in an Antares study

## Usage

``` r
editDistrict(
  name,
  caption = NULL,
  comments = NULL,
  apply_filter = NULL,
  add_area = NULL,
  remove_area = NULL,
  output = NULL,
  opts = simOptions()
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

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`createDistrict()`](createDistrict.md),
[`removeDistrict()`](removeDistrict.md)

## Examples

``` r
if (FALSE) { # \dontrun{
editDistrict(
  name = "my_existing_district",
  comments = "This is my district",
  add_area = c("area1", "area3", "area5"),
  opts = simOptions()
)
} # }
```
