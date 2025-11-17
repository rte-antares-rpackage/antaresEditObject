# Update optimization parameters of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Update optimization parameters and other preferences of an Antares study

## Usage

``` r
updateOptimizationSettings(
  simplex.range = NULL,
  transmission.capacities = NULL,
  include.constraints = NULL,
  include.hurdlecosts = NULL,
  include.tc.min.stable.power = NULL,
  include.tc.min.up.down.time = NULL,
  include.dayahead = NULL,
  include.strategicreserve = NULL,
  include.spinningreserve = NULL,
  include.primaryreserve = NULL,
  include.exportmps = NULL,
  solver.log = NULL,
  power.fluctuations = NULL,
  shedding.strategy = NULL,
  shedding.policy = NULL,
  unit.commitment.mode = NULL,
  number.of.cores.mode = NULL,
  renewable.generation.modelling = NULL,
  day.ahead.reserve.management = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- simplex.range:

  week or day

- transmission.capacities:

  true, false or infinite (since v8.4 can also take : local-values,
  null-for-all-links, infinite-for-all-links, null-for-physical-links,
  infinite-for-physical-links)

- include.constraints:

  true or false

- include.hurdlecosts:

  true or false

- include.tc.min.stable.power:

  true or false

- include.tc.min.up.down.time:

  true or false

- include.dayahead:

  true or false

- include.strategicreserve:

  true or false

- include.spinningreserve:

  true or false

- include.primaryreserve:

  true or false

- include.exportmps:

  true or false (since v8.3.2 can take also : none, optim-1, optim-2,
  both-optims)

- solver.log:

  true or false (available for version \>= 8.8)

- power.fluctuations:

  free modulations, minimize excursions or minimize ramping

- shedding.strategy:

  share margins

- shedding.policy:

  shave peaks (accurate shave peaks for study \>= v9.2)or minimize
  duration

- unit.commitment.mode:

  fast or accurate

- number.of.cores.mode:

  minimum, low, medium, high or maximum

- renewable.generation.modelling:

  aggregated or clusters

- day.ahead.reserve.management:

  global

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{

updateOptimizationSettings(
  simplex.range = "week", 
  power.fluctuations = "minimize ramping"
)

} # }
```
