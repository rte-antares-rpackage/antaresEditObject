# Create a cluster

![Antares API OK](figures/badge_api_ok.svg) (thermal clusters only)

Create a new thermal or RES (renewable energy source) cluster.

## Usage

``` r
createCluster(
  area,
  cluster_name,
  group = "Other",
  ...,
  list_pollutants = NULL,
  time_series = NULL,
  prepro_data = NULL,
  prepro_modulation = NULL,
  add_prefix = TRUE,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)

createClusterRES(
  area,
  cluster_name,
  group = default_group(opts),
  ...,
  time_series = NULL,
  add_prefix = TRUE,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to create the cluster.

- cluster_name:

  Name for the cluster, it will prefixed by area name, unless you set
  `add_prefix = FALSE`.

- group:

  Group of the cluster, depends on cluster type:

  - thermal cluster, one of: Gas, Hard coal, Lignite, Mixed fuel,
    Nuclear, Oil, Other, Other 2, Other 3, Other 4(**dynamic name for
    Antares version \>= 9.3**).

  - renewable cluster, one of: Wind Onshore, Wind Offshore, Solar
    Thermal, Solar PV, Solar Rooftop, Other RES 1, Other RES 2, Other
    RES 3, Other RES 4(**dynamic name for Antares version \>= 9.3.
    Default group by version: Other RES 1 if \<930, Other if \>=930**).

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

- overwrite:

  Logical, overwrite the cluster or not.

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

[`editCluster()`](editCluster.md) or
[`editClusterRES()`](editCluster.md) to edit existing clusters,
[`removeCluster()`](removeCluster.md) or
[`removeClusterRES()`](removeCluster.md) to remove clusters.

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)
library(antaresEditObject)

# Create a cluster :
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  group = "other", 
  unitcount = 1L, # or as.integer(1)
  marginal_cost = 50
)
# by default, cluster name is prefixed 
# by the area name
levels(readClusterDesc()$cluster)
# > "fr_my_cluster"

# To prevent this, use `add_prefix`
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  add_prefix = FALSE,
  group = "other", 
  marginal_cost = 50
)
levels(readClusterDesc()$cluster)
# > "my_cluster"


# Create a RES cluster :
createClusterRES(
  area = "fr", 
  cluster_name = "my_cluster_res",
  group = "other", 
  unitcount = 1L, # or as.integer(1)
  nominalcapacity = 50,
  ts_interpretation = "power-generation"
) 

# You can also specify that the Time-Series of the RES cluster are
# production factors :
createClusterRES(
  area = "fr", 
  cluster_name = "my_cluster_res",
  group = "other", 
  unitcount = 1L, # or as.integer(1)
  nominalcapacity = 50,
  ts_interpretation = "production-factor"
)


# Pre-process data : 

# this is the default data :
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  prepro_data = matrix(
    data = c(rep(1, times = 365 * 2),
             rep(0, times = 365 * 4)), 
    ncol = 6
  )
)

# with a data.frame
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  prepro_data = data.frame(
    v1 = rep(7, 365), # column name does not matter
    v2 = rep(27, 365),
    v3 = rep(0.05, 365),
    v4 = rep(0.12, 365),
    v5 = rep(0, 365),
    v6 = rep(1, 365)
  )
)


# Pre-process modulation : 
# this is the default data
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  prepro_modulation = matrix(
    data = c(rep(1, times = 365 * 24 * 3),
             rep(0, times = 365 * 24 * 1)),
    ncol = 4
  )
)

# with a data.frame
createCluster(
  area = "fr", 
  cluster_name = "my_cluster",
  prepro_modulation = data.frame(
    var1 = rep(0, 8760), # column name does not matter
    var2 = rep(1, 8760),
    var3 = rep(0, 8760),
    var4 = rep(1, 8760)
  )
)

} # }
```
