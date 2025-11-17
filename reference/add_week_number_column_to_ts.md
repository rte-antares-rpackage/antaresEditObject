# Add week number column to a data.time of time series type

If timeId column exists, add a week number column. A week is 168
consecutive hours (= 24 \* 7).

## Usage

``` r
add_week_number_column_to_ts(xts)
```

## Arguments

- xts:

  a data.table of time series type.

## Value

the data.table xts with a new column week.
