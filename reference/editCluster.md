# Edit an existing cluster

![Antares API OK](figures/badge_api_ok.svg) (thermal clusters only)

Edit parameters, pre-process data and time series of an existing
cluster, thermal or RES (renewable energy source).

## Usage

``` r
editCluster(
  area,
  cluster_name,
  ...,
  list_pollutants = NULL,
  time_series = NULL,
  prepro_data = NULL,
  prepro_modulation = NULL,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)

editClusterRES(
  area,
  cluster_name,
  ...,
  time_series = NULL,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to create the cluster.

- cluster_name:

  Name for the cluster, it will prefixed by area name, unless you set
  `add_prefix = FALSE`.

- ...:

  Parameters to write in the Ini file. Careful! Some parameters must be
  set as `integers` to avoid warnings in Antares, for example, to set
  `unitcount`, you'll have to use `unitcount = 1L`.

- list_pollutants:

  `list` named with specific pollutants (only for Antares version \>=
  860)

- time_series:

  the "ready-made" 8760-hour time-series available for simulation
  purposes.

- prepro_data:

  Pre-process data, a `data.frame` or `matrix`, default is a matrix with
  365 rows and 6 columns.

- prepro_modulation:

  Pre-process modulation, a `data.frame` or `matrix`, if specified, must
  have 8760 rows and 1 or 4 columns.

- add_prefix:

  If `TRUE` (the default), `cluster_name` will be prefixed by area name.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

Parameter `list_pollutants` is only available for Antares studies \>=
v8.6.0.

You must provide named `list` (numerical values or NULL ) :

`list( "nh3"= 0.25, "nox"= 0.45, "pm2_5"= 0.25, "pm5"= 0.25, "pm10"= 0.25, "nmvoc"= 0.25, "so2"= 0.25, "op1"= 0.25, "op2"= 0.25, "op3"= 0.25, "op4"= 0.25, "op5"= NULL, "co2"= NULL)`

## See also

[`createCluster()`](createCluster.md) or
[`createClusterRES()`](createCluster.md) to create new clusters,
[`removeCluster()`](removeCluster.md) or
[`removeClusterRES()`](removeCluster.md) to remove clusters.

## Examples

``` r
if (FALSE) { # \dontrun{

# Update only nominalCapacity for an existing cluster
editCluster(
  area = "myarea", 
  cluster_name = "mycluster", 
  nominalcapacity = 10600.000
)

} # }
```
