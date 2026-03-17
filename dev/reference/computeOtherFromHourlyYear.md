# Compute daily, weekly, monthly and annual mc-ind from hourly data for one year. (new)

Compute daily, weekly, monthly and annual mc-ind from hourly data for
one year. (new)

## Usage

``` r
computeOtherFromHourlyYear(
  mcYear,
  type,
  areas = "all",
  opts = simOptions(),
  timeStep = c("daily", "monthly", "annual", "weekly"),
  writeOutput = FALSE
)
```

## Arguments

- mcYear:

  vector of years to compute

- type:

  type of data (areas, links, clusters, clustersRes)

- areas:

  vector of areas. links type will use getLinks() to get data.

- opts:

  study opts

- timeStep:

  timestep of aggregation (daily, monthly and annual, NO weekly)

- writeOutput:

  boolean to write data in mc-ind folder

## Note

Recommended only with studies spanning on two years.

## See also

[`computeOtherFromHourlyMulti`](computeOtherFromHourlyMulti.md)
