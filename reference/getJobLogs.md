# Retrieve job log from API

Retrieve job log from API

## Usage

``` r
getJobLogs(job_id, opts = antaresRead::simOptions())
```

## Arguments

- job_id:

  The job identifier.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

Logs as character string.

## Examples

``` r
if (FALSE) { # \dontrun{

antaresRead::setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "39c604fc-687f-46c4-9fa6-59b57ff9c8d1",
  token = NULL,
  simulation = "input"
)
job <- runSimulation()
getJobLogs(job)

} # }
```
