# Write Economic Options

![Antares API OK](figures/badge_api_ok.svg)

This function allows to create or edit economic options. Areas/options
present in the input dataframe are edited, while all other values are
left unchanged.

## Usage

``` r
writeEconomicOptions(x, opts = antaresRead::simOptions())
```

## Arguments

- x:

  A dataframe. Must contain an `area` column listing some (but not
  necessarily all) areas of the study. Can contain up to 7 other columns
  among: `average_unsupplied_energy_cost`,
  `spread_unsupplied_energy_cost`, `average_spilled_energy_cost`,
  `spread_spilled_energy_cost`, (numeric columns),
  `non_dispatchable_power`, `dispatchable_hydro_power` and
  `other_dispatchable_power` (logical columns).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)

# Set simulation path
setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Write some economic options for areas a, b and c
writeEconomicOptions(data.frame(
  area = c("a", "b", "c"),
  dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
  spread_unsupplied_energy_cost = c(0.03, 0.024, 0.01),
  average_spilled_energy_cost = c(10, 8, 8),
  stringsAsFactors = FALSE
))

} # }
```
