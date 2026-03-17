# Replicate a data.table as many times as needed to get the same number of time series between 2 data.tables

Replicate a data.table as many times as needed to get the same number of
time series between 2 data.tables

## Usage

``` r
replicate_missing_ts(xts, yts)
```

## Arguments

- xts:

  a data.table of time series type to replicate if necessary.

- yts:

  a data.table of time series type to use as reference to match its
  number of time series.

## Value

the data.table x replicated to match the number of time series of y.
