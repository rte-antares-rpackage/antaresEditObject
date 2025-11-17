# Variant management with API

``` r
 # CRAN limite CPU usage
data.table::setDTthreads(2)
library(antaresEditObject)
#> Loading required package: antaresRead
```

The API behind [Antares
Web](https://antares-web.readthedocs.io/en/latest/) comes with a Variant
Manager allowing to edit a study. Functions from {antaresEditObject} can
be used send commands to the API or generate commands to be sent to the
API.

## Path to simulation and variant creation

First we need to declare which study we are going to use:

``` r
antaresRead::setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "70a08fae-da67-444a-b2ed-df4c0f956a31", 
  token = NULL, 
  simulation = "input"
)
```

Then we can create a new variant from our study or use one created
through the web interface:

``` r
# Create new variant
createVariant("variant-1")

# use an existing one
useVariant("variant-2")
```

A third option is to mock the web server behavior, it can be useful if
we are offline or if we just want to generate API commands to be use
afterwards:

``` r
mockSimulationAPI()
```

## API editing mode

{antaresEditObject} allow to use two modes to use the API:

- **async**: record all API calls, but nothing is sent to the server,
  commands can be retrieved after calling {antaresEditObject}’s
  functions
- **sync**: send query to the API each time a function is used (commands
  are also internally recorded if needed).

``` r
setAPImode("async")
# or
setAPImode("sync")
```

Default is to used **async** mode. When using
[`mockSimulationAPI()`](../reference/mockSimulationAPI.md) only
**async** mode is available.

## Get / export variant management commands

Variant commands generated after calling functions like
[`createArea()`](../reference/createArea.md),
[`createLink()`](../reference/createLink.md), … can be retrieved at all
time with:

``` r
getVariantCommands()
```

Last command generated can be viewed with:

``` r
getVariantCommands(last = TRUE)
# or use a numeric to get the last N commands
getVariantCommands(last = 3)
```

You can also filter type of commands with:

``` r
getVariantCommands(actions = "create_area")
```

Export commands with:

``` r
writeVariantCommands("path/to/commands.json")
```

## Usage example

Below are listed all functions from {antaresEditObject} that can be used
with the API. These functions will include the following badge in their
documentation:

![](https://img.shields.io/badge/Antares%20API-OK-green)

### Create an area

Create a new area:

``` r
createArea(name = "area01")
#> ℹ Command create_area registered, see all commands with `getVariantCommands()`
#> ℹ Command update_area_ui registered, see all commands with `getVariantCommands()`
createArea(name = "area02")
#> ℹ Command create_area registered, see all commands with `getVariantCommands()`
#> ℹ Command update_area_ui registered, see all commands with `getVariantCommands()`
createArea(name = "area03")
#> ℹ Command create_area registered, see all commands with `getVariantCommands()`
#> ℹ Command update_area_ui registered, see all commands with `getVariantCommands()`
getVariantCommands()
#> [
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area01"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area01",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   },
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area02"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area02",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   },
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area03"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area03",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   }
#> ]
```

Create a second area with some default parameters:

``` r
createArea(
  name = "area04", 
  filtering = filteringOptions(filter_synthesis = c("hourly", "daily"))
)
#> ℹ Command create_area registered, see all commands with `getVariantCommands()`
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
#> ℹ Command update_area_ui registered, see all commands with `getVariantCommands()`
getVariantCommands()
#> [
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area01"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area01",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   },
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area02"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area02",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   },
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area03"
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area03",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   },
#>   {
#>     "action": "create_area",
#>     "args": {
#>       "area_name": "area04"
#>     }
#>   },
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "input/areas/area04/optimization/filtering",
#>       "data": {
#>         "filter-synthesis": "hourly, daily",
#>         "filter-year-by-year": "hourly, daily, weekly, monthly, annual"
#>       }
#>     }
#>   },
#>   {
#>     "action": "update_area_ui",
#>     "args": [
#>       {
#>         "area_id": "area04",
#>         "area_ui": {
#>           "x": 0,
#>           "y": 0,
#>           "layer_x": {
#>             "0": 0
#>           },
#>           "layer_y": {
#>             "0": 0
#>           },
#>           "color_rgb": [230, 108, 44],
#>           "layer_color": {
#>             "0": "230, 108, 44"
#>           }
#>         },
#>         "layer": "0"
#>       }
#>     ]
#>   }
#> ]
```

You can also edit an area or remove it:

``` r
createArea(name = "area000")
#> ℹ Command create_area registered, see all commands with `getVariantCommands()`
#> ℹ Command update_area_ui registered, see all commands with `getVariantCommands()`
# editArea(name = "area000", ...)
removeArea(name = "area000")
#> ℹ Command remove_area registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "remove_area",
#>     "args": {
#>       "id": "area000"
#>     }
#>   }
#> ]
```

### Create a link

Create a new link between two areas like this:

``` r
createLink(from = "area01", to = "area02")
#> ℹ Command create_link registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
createLink(from = "area01", to = "area03")
#> ℹ Command create_link registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
getVariantCommands(last = 2)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area03_direct",
#>       "matrix": [[0],[0],[0],[0],[0],[0]] [truncated]...
#>     }
#>   },
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area03_indirect",
#>       "matrix": [[0],[0],[0],[0],[0],[0]] [truncated]...
#>     }
#>   }
#> ]
```

Edit an existing link with:

``` r
editLink(
  from = "area01", 
  to = "area02",
  dataLink = matrix(data = c(rep(9, 8760*2), rep(6, 8760*6)), ncol = 8)
)
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
getVariantCommands(last = 2)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_direct",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   },
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_indirect",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   }
#> ]
```

Remove a link with:

``` r
removeLink(from = "area01", to = "area03")
#> Link doesn't exist
#> NULL
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_indirect",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   }
#> ]
```

### Create a cluster

Create a new cluster with:

``` r
createCluster(
  area = "area01", 
  cluster_name = "clus01"
)
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_indirect",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   }
#> ]
```

With more parameters:

``` r
createCluster(
  area = "area01", 
  cluster_name = "clus02",
  unitcount = 1L,
  marginal_cost = 50,
  ts_interpretation = "production-factor",
  group = "Nuclear",
  add_prefix = FALSE,
  prepro_data = matrix(
    data = c(rep(9, times = 365 * 2),
             rep(7, times = 365 * 4)), 
    ncol = 6
  ),
  prepro_modulation = matrix(
    data = c(rep(8, times = 365 * 24 * 3),
             rep(6, times = 365 * 24 * 1)),
    ncol = 4
  ),
  time_series = matrix(
    data = c(rep(22, times = 365 * 24 * 8),
             rep(44, times = 365 * 24 * 4)),
    ncol = 12
  )
)
getVariantCommands(last = 2)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_direct",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   },
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/links/area01/capacities/area02_indirect",
#>       "matrix": [[9],[9],[9],[9],[9],[9]] [truncated]...
#>     }
#>   }
#> ]
```

Edit a cluster with:

``` r
createCluster(
  area = "area02", 
  cluster_name = "clus02"
)
editCluster(
  area = "area02", 
  cluster_name = "clus02", 
  unitcount = 5L
)
getVariantCommands(last = TRUE)
```

Remove a cluster with
([`removeCluster()`](../reference/removeCluster.md) has been updated
with endpoint and no longer uses an api command ):

``` r
createCluster(
  area = "area02", 
  cluster_name = "clus000"
)
removeCluster(
  area = "area02", 
  cluster_name = "clus000"
)
getVariantCommands(last = TRUE)
```

### Create a binding constraint

Create a new binding constraint with:

``` r
createBindingConstraint(
  name = "myconstraint",
  values = NULL,
  enabled = FALSE,
  timeStep = "hourly",
  operator = "both",
  coefficients = c("area01%area02" = 1)
)
#> ℹ Command create_binding_constraint registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "create_binding_constraint",
#>     "args": {
#>       "name": "myconstraint",
#>       "enabled": false,
#>       "time_step": "hourly",
#>       "operator": "both",
#>       "filter_year_by_year": "hourly, daily, weekly, monthly, annual",
#>       "filter_synthesis": "hourly, daily, weekly, monthly, annual",
#>       "coeffs": {
#>         "area01%area02": [
#>           1
#>         ]
#>       }
#>     }
#>   }
#> ]
```

You can edit a binding constraint with
[`editBindingConstraint()`](../reference/editBindingConstraint.md) and
remove one with
[`removeBindingConstraint()`](../reference/removeBindingConstraint.md).

### Update settings

Update general settings:

``` r
updateGeneralSettings(mode = "Adequacy", generate = c("thermal", "hydro"))
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
getVariantCommands(last = 2)
#> [
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/general/mode",
#>       "data": "Adequacy"
#>     }
#>   },
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/general/generate",
#>       "data": "thermal, hydro"
#>     }
#>   }
#> ]
```

Update input settings:

``` r
updateInputSettings(import = c("hydro", "thermal"))
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/input/import",
#>       "data": "hydro, thermal"
#>     }
#>   }
#> ]
```

Update optimization settings:

``` r
updateOptimizationSettings(
  simplex.range = "week",
  power.fluctuations = "minimize ramping"
)
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
getVariantCommands(last = 2)
#> [
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/optimization/simplex-range",
#>       "data": "week"
#>     }
#>   },
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/other preferences/power-fluctuations",
#>       "data": "minimize ramping"
#>     }
#>   }
#> ]
```

Update output settings:

``` r
updateOutputSettings(
  synthesis = TRUE,
  storenewset = FALSE,
  archives = c("load", "wind")
)
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/generaldata/output/archives",
#>       "data": "load, wind"
#>     }
#>   }
#> ]
```

### Scenario builder

Read data from scenario builder (here’s it’s empty since we’re mocking
the API):

``` r
readScenarioBuilder()
#> list()
```

Create new data to use as scenario builder, since we’re mocking the API
parameters must be set explicitly, otherwise thez are retrieved from the
study.

``` r
my_scenario <- scenarioBuilder(n_scenario = 3, areas = c("area01", "area02"), n_mc = 10)
my_scenario
#>        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> area01 "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"  
#> area02 "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"
```

Then you can update the scenario builder itself:

``` r
updateScenarioBuilder(ldata = my_scenario, series = "load")
#> ℹ Command update_config registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "update_config",
#>     "args": {
#>       "target": "settings/scenariobuilder/Default Ruleset",
#>       "data": {"l,area01,0":"1","l,area01,1":"2","l,area01,2":"3","l,area01,3":"1","l,area01,4":"2","l,area01,5":"3"} [truncated]...
#>     }
#>   }
#> ]
```

### Other functions

Write input time series:

``` r
writeInputTS("area01", type = "solar", data = matrix(rep(4, 8760*2), nrow = 8760))
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/solar/series/solar_area01",
#>       "matrix": [[4,4],[4,4],[4,4],[4,4],[4,4],[4,4]] [truncated]...
#>     }
#>   }
#> ]
```

Write water values:

``` r
writeWaterValues("area01", data = matrix(rep(0, 365*101), nrow = 365))
#> ℹ Command replace_matrix registered, see all commands with `getVariantCommands()`
getVariantCommands(last = TRUE)
#> [
#>   {
#>     "action": "replace_matrix",
#>     "args": {
#>       "target": "input/hydro/common/capacity/waterValues_area01",
#>       "matrix": [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]] [truncated]...
#>     }
#>   }
#> ]
```
