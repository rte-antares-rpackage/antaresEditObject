# Nodal optimization parameters for creating an area

Nodal optimization parameters for creating an area

## Usage

``` r
nodalOptimizationOptions(
  non_dispatchable_power = TRUE,
  dispatchable_hydro_power = TRUE,
  other_dispatchable_power = TRUE,
  spread_unsupplied_energy_cost = 0,
  spread_spilled_energy_cost = 0,
  average_unsupplied_energy_cost = 0,
  average_spilled_energy_cost = 0
)
```

## Arguments

- non_dispatchable_power:

  logical, default to FALSE

- dispatchable_hydro_power:

  logical, default to FALSE

- other_dispatchable_power:

  logical, default to FALSE

- spread_unsupplied_energy_cost:

  numeric, default to 0

- spread_spilled_energy_cost:

  numeric, default to 0

- average_unsupplied_energy_cost:

  numeric, default to 0

- average_spilled_energy_cost:

  numeric, default to 0

## Value

a named list

## Examples

``` r
nodalOptimizationOptions()
#> $`non-dispatchable-power`
#> [1] TRUE
#> 
#> $`dispatchable-hydro-power`
#> [1] TRUE
#> 
#> $`other-dispatchable-power`
#> [1] TRUE
#> 
#> $`spread-unsupplied-energy-cost`
#> [1] 0
#> 
#> $`spread-spilled-energy-cost`
#> [1] 0
#> 
#> $unserverdenergycost
#> [1] 0
#> 
#> $spilledenergycost
#> [1] 0
#> 
```
