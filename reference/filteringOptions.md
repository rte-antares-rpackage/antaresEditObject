# Output profile options for creating an area

Output profile options for creating an area

## Usage

``` r
filteringOptions(
  filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
  filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual")
)
```

## Arguments

- filter_synthesis:

  Character, vector of time steps used in the output synthesis, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

- filter_year_by_year:

  Character, vector of time steps used in the output year-by-year, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

## Value

a named list

## Examples

``` r
filteringOptions(
  filter_synthesis=c("hourly","daily"),
  filter_year_by_year=c("weekly","monthly")
)
#> $`filter-synthesis`
#> [1] "hourly, daily"
#> 
#> $`filter-year-by-year`
#> [1] "weekly, monthly"
#> 
```
