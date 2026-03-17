# Compute daily, weekly, monthly and annual mc-ind from hourly data.

![Antares API NO](figures/badge_api_no.svg)

Compute daily, weekly, monthly and annual mc-ind from hourly data.

## Usage

``` r
computeTimeStampFromHourly(
  opts,
  mcYears = "all",
  nbcl = 8,
  verbose = 1,
  type = c("areas", "links", "clusters")
)
```

## Arguments

- opts:

  opts simulation path.

- mcYears:

  mcYears to compute.

- nbcl:

  number of thread for parallel computing.

- verbose:

  verbose for execution.

- type:

  type of file to compute.

## Note

Deprecated on studies v8 or higher.

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresEditObject)
opts <- setSimulationPath("my_study")
computeTimeStampFromHourly(opts)

} # }
```
