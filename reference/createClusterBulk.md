# Create serial thermal cluster

For each area, the thermal cluster data are generated :

- Writing `.ini` files

- Writing time_series files

- Writing prepro_data files

- Writing prepro_modulation files

## Usage

``` r
createClusterBulk(
  cluster_object,
  area_zone,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- cluster_object:

  `list` mutiple list containing the parameters for writing each cluster

- area_zone:

  `character` name of area to create cluster

- add_prefix:

  `logical` prefix cluster name with area name

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

`list` containing meta information about the simulation

## Details

see the example to write a cluster object, see the original function
[`createCluster()`](createCluster.md)

Structure of `cluster_object` :

The list must be structured with named items

- `parameter` : `list` of paramaters to write in .ini file

- `overwrite` : `logical` to choose to overwrite an existing cluster (if
  not present, set to `FALSE`)

- `time_series` : `matrix` or `data.frame` the "ready-made" 8760-hour
  time-series

- `prepro_data` : `matrix` or `data.frame` Pre-process data

- `prepro_modulation` : `matrix` or `data.frame` Pre-process modulation

Details for sublist `cluster_object[["parameter"]]` :

- `name` : Name for the cluster, it will prefixed by area name, unless
  you set add_prefix = FALSE

- `group` : Group of the cluster, depends on cluster type

- `...` : Parameters to write in the Ini file

## Examples

``` r
if (FALSE) { # \dontrun{

# /!\/!\/!\ use or create a study /!\/!\/!\

# data preparation for sutructures
ts <- matrix(rep(c(0, 8000), each = 24*364), 
             ncol = 2)

df_pd <- matrix(rep(c(1, 1, 1, 0), each = 24*365), 
                ncol = 4)

df_pm <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), 
                ncol = 4)


# Example cluster object
zone_test_1 <- list(
  `CCGT old 1`= list(
  parameter= list(
  name= "CCGT old 1",
  group = "Other",
  unitcount= 10L,
  nominalcapacity= 100,
   enabled= "true",
   `min-stable-power`= 80L,
   `min-up-time`= 20L,
   `min-down_time`= 30L),
   overwrite= TRUE,
   time_series = ts_8760,
   prepro_data = df_pd,
   prepro_modulation = df_pm))
 
 # overwrite existing cluster
zone_test_2 <- list(
 `PEAK`= list(parameter= list(
   name= "PEAK",
   group = "Other"),
   overwrite= TRUE,
   time_series = ts,
   prepro_data = df_pd,
   prepro_modulation = df_pm))

# Create multiple areas with multiple clusters
list_areas <- antaresRead::getAreas()[1:5]

lapply(list_areas, createClusterBulk,
cluster_object = c(zone_test_1, zone_test_2),
add_prefix = TRUE)

} # }
```
