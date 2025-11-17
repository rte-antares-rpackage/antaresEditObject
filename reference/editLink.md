# Edit a link between two areas

![Antares API OK](figures/badge_api_ok.svg)

Edit a link between two areas in an Antares study.

## Usage

``` r
editLink(
  from,
  to,
  hurdles_cost = NULL,
  transmission_capacities = NULL,
  asset_type = NULL,
  display_comments = NULL,
  filter_synthesis = NULL,
  filter_year_by_year = NULL,
  dataLink = NULL,
  tsLink = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- from, to:

  The two areas linked together.

- hurdles_cost:

  Logical, which is used to state whether (linear) transmission fees
  should be taken into account or not in economy and adequacy
  simulations

- transmission_capacities:

  Character, one of `enabled`, `ignore` or `infinite`, which is used to
  state whether the capacities to consider are those indicated in
  8760-hour arrays or if zero or infinite values should be used instead
  (actual values / set to zero / set to infinite)

- asset_type:

  Character, one of `ac`, `dc`, `gas`, `virt` or `other`. Used to state
  whether the link is either an AC component (subject to Kirchhoff’s
  laws), a DC component, or another type of asset.

- display_comments:

  Logical, display comments or not.

- filter_synthesis:

  Character, vector of time steps used in the output synthesis, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

- filter_year_by_year:

  Character, vector of time steps used in the output year-by-year, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

- dataLink:

  See Details section below.

- tsLink:

  Transmission capacities time series. First N columns are direct TS,
  following N are indirect ones.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

See [`createLink()`](createLink.md) for more documentation

## See also

[`createLink()`](createLink.md), [`removeLink()`](removeLink.md)

## Examples

``` r
if (FALSE) { # \dontrun{
editLink(
  from = "area1",
  to = "area2",
  transmission_capacities = "infinite",
  filter_synthesis = c("hourly","daily"),
  filter_year_by_year = c("weekly","monthly")
)
} # }
```
