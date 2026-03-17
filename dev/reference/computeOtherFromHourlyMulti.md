# Compute daily, weekly, monthly and annual mc-ind from hourly data multiyear. (new)

Compute daily, weekly, monthly and annual mc-ind from hourly data
multiyear. (new)

## Usage

``` r
computeOtherFromHourlyMulti(
  opts = simOptions(),
  areas = "all",
  type = c("areas", "links", "clusters"),
  timeStep = c("daily", "monthly", "annual", "weekly"),
  mcYears = simOptions()$mcYears,
  writeOutput = FALSE,
  nbcl = 8,
  verbose = FALSE
)
```

## Arguments

- opts:

  study opts

- areas:

  vector of areas

- type:

  type of aggregation

- timeStep:

  timestep of aggregation (daily, monthly and annual, NO weekly)

- mcYears:

  vector of years to compute

- writeOutput:

  boolean to write data in mc-ind folder

- nbcl:

  number of cpu cores for parallelization

- verbose:

  logical for printing output

## Note

Recommended only with studies spanning on two years.

## See also

[`computeOtherFromHourlyYear`](computeOtherFromHourlyYear.md)
