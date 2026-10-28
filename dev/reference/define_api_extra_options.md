# Output profile options for running a simulation in API mode

Output profile options for running a simulation in API mode

## Usage

``` r
define_api_extra_options(
  launcher = NULL,
  solver_preset = NULL,
  solver_version = NULL,
  run_at = NULL
)
```

## Arguments

- launcher:

  Name of the launcher.

- solver_preset:

  Name of the solver.

- solver_version:

  Version of the solver.

- run_at:

  When the simulation should be run. Format YYYY-MM-DD hh-mm-ss. Time
  provided in GMT.

## Value

a named list

## Examples

``` r
define_api_extra_options(
  launcher="calin2opf",
  solver_version="9.4"
)
#> $launcher
#> [1] "calin2opf"
#> 
#> $solver_preset
#> NULL
#> 
#> $solver_version
#> [1] "9.4"
#> 
#> $run_at
#> NULL
#> 
```
