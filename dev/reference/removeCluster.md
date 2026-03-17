# Remove a cluster

![Antares API OK](figures/badge_api_ok.svg)

Delete cluster(s), thermal, renewable (renewable energy source) or
short-term storage, along with all its data (properties + TS).

## Usage

``` r
removeCluster(
  area,
  cluster_name,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)

removeClusterRES(
  area,
  cluster_name,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)

removeClusterST(
  area,
  cluster_name,
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

- add_prefix:

  If `TRUE` (the default), `cluster_name` will be prefixed by area name.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

[`createCluster()`](createCluster.md),
[`createClusterRES()`](createCluster.md) or
[`createClusterST()`](createClusterST.md) to create new clusters,
[`editCluster()`](editCluster.md) or
[`editClusterRES()`](editCluster.md) or
[`editClusterST()`](editClusterST.md) to edit existing clusters.

## Examples

``` r
if (FALSE) { # \dontrun{
createCluster(
  area = "fr", 
  cluster_name = "fr_gas",
  group = "other", 
  `marginal-cost` = 50
)

removeCluster(area = "fr", cluster_name = "fr_gas")
} # }
```
