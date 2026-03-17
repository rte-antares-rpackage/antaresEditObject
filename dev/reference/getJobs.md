# Retrieve API jobs

Retrieve API jobs

## Usage

``` r
getJobs(job_id = NULL, opts = antaresRead::simOptions())
```

## Arguments

- job_id:

  The job identifier, if `NULL` (default), retrieve all jobs.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

A `data.table` with information about jobs.

## Examples

``` r
if (FALSE) { # \dontrun{

getJobs()

} # }
```
