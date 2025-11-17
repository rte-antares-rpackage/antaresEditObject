# User guide

## Prerequisites

First it’s necessary to load the package:

``` r
 # CRAN limite CPU usage
data.table::setDTthreads(2)
library(antaresEditObject)
```

You need to set the path to an Antares study in “input” mode:

``` r
antaresRead::setSimulationPath(path = "path/to/study", simulation = "input")
```

Or you can simply create a new study:

``` r
createStudy("path/to/study")
```

## Save study

Before modifying your study, you can save it in an archive:

``` r
backupStudy(what = "input")
```

This will create a `.tar.gz` file in your study folder.

## Create a new area

You can create a new area with:

``` r
createArea(name = "myarea")

# The new area should appear here:
antaresRead::getAreas()
```

You can specify the localization of the area on the map, and also its
color.

There are two helper functions for area parameters:

- [`filteringOptions()`](../reference/filteringOptions.md) for filtering
  options, like `filter-year-by-year`
- [`nodalOptimizationOptions()`](../reference/nodalOptimizationOptions.md)
  for nodal optimizations options.

## Create a new cluster

You can initialize a cluster with some parameters:

``` r
createCluster(
  area = "myarea", 
  cluster_name = "myareacluster",
  group = "other",
  unitcount = 1,
  nominalcapacity = 8400,
  `min-down-time` = 0,
  `marginal-cost` = 0.010000,
  `market-bid-cost` = 0.010000
)
```

You can also edit the settings of an existing cluster:

``` r
editCluster(
  area = "myarea", 
  cluster_name = "myareacluster", 
  nominalcapacity = 10600.000
)
```

## Create a new link

``` r
createLink(
  from = "area1", 
  to = "area2", 
  propertiesLink = propertiesLinkOptions(
    hurdles_cost = FALSE,
    transmission_capacities = "enabled"
  ), 
  dataLink = NULL
)
```

You can edit the settings of an existing link:

``` r
editLink(
  from = "area1",
  to = "area2",
  transmission_capacities = "infinite"
)
```

## Create a binding constraint

``` r
createBindingConstraint(
  name = "myconstraint", 
  values = matrix(data = c(rep(c(19200, 0, 0), each = 366)), ncol = 3), 
  enabled = FALSE, 
  timeStep = "daily",
  operator = "both",
  coefficients = c("fr%myarea" = 1)
)
```

## Create several Pumped Storage Power plant (PSP)

``` r
pspData <- data.frame(
  area = c("a", "b"), 
  installedCapacity = c(800,900)
)

createPSP(
  areasAndCapacities = pspData, 
  efficiency = 0.75
)
```

## Create several Demand Side Response (DSR)

``` r
dsrData <- data.frame(
  area = c("a", "b"),
  unit = c(10,20), 
  nominalCapacity = c(100, 120),
  marginalCost = c(52, 65),
  hour = c(3, 7)
)
  
createDSR(dsrData)
```

## Update general settings

For example, set the output of simulation year by year, and limit the
number of Monte-Carlo years to 10:

``` r
updateGeneralSettings(year.by.year = TRUE, nbyears = 10)
```

## Remove methods

You can remove areas, links, clusters and binding constraints from input
folder with `remove*` functions, e.g.:

``` r
removeArea("myarea")
```

## Run Time-Series Generator

First, update general settings to activate time series to generate:

``` r
updateGeneralSettings(generate = "thermal")
```

Then run TS-generator:

``` r
runTsGenerator(
  path_solver = "C:/path/to/antares-solver.exe", 
  show_output_on_console = TRUE
)
```

## Run an Antares simulation

Launch an Antares simulation from R:

``` r
runSimulation(
  name = "myAwesomeSimulation", 
  mode = "economy",
  path_solver = "C:/path/to/antares-solver.exe", 
  show_output_on_console = TRUE
)
```

## Read a time series, update it and write it

To update an existing time series and write it, you can use the
following commands :

``` r
# Filepath of the study, version >= 820
my_study <- file.path("", "", "")
opts <- setSimulationPath(my_study, simulation ="input")
opts$timeIdMax <- 8760

# Links, use only one link
my_link <- as.character(getLinks()[1])
ts_input <- readInputTS(linkCapacity = my_link, opts = opts)

# Sort the data to ensure its reliability
data.table::setorder(ts_input, cols = "tsId", "timeId") 

# Reshape to wide format : writeInputTS expects a 8760 * N matrix 
metrics <- c("transCapacityDirect", "transCapacityIndirect")
ts_input_reformatted <- data.table::dcast(ts_input,
                                          timeId ~ tsId,
                                          value.var = metrics
                                          )
# Add a value my_param to your matrix
my_param <- 123
writeInputTS(data = ts_input_reformatted[,2:ncol(ts_input_reformatted)] + my_param,
             type = "tsLink",
             link = my_link,
             overwrite = TRUE,
             opts = opts
             )


# Thermal, use only one area and one cluster
my_area <- "zone"
my_cluster <- "mon_cluster"
ts_input <- readInputTS(thermalAvailabilities = my_area, opts = opts)
ts_input <- ts_input[cluster == paste0(my_area,"_",my_cluster)]

# Sort the data to ensure its reliability
data.table::setorder(ts_input, cols = "tsId", "timeId")

# Reshape to wide format : writeInputTS expects a 8760 * N matrix 
metrics <- c("ThermalAvailabilities")
ts_input_reformatted <- data.table::dcast(ts_input,
                                          timeId ~ tsId,
                                          value.var = metrics
                                          )

# Add a value my_param to your matrix
my_param <- 1000
editCluster(area = my_area,
            cluster_name = my_cluster,
            time_series = ts_input_reformatted[,2:ncol(ts_input_reformatted)] + my_param,
            opts = opts
            )


# Run of River, use only one area
my_area <- "zone"
ts_input <- readInputTS(ror = my_area, opts = opts)

# Sort the data to ensure its reliability
data.table::setorder(ts_input, cols = "tsId", "timeId")

# Reshape to wide format : writeInputTS expects a 8760 * N matrix 
metrics <- c("ror")
ts_input_reformatted <- data.table::dcast(ts_input,
                                          timeId ~ tsId,
                                          value.var = metrics
)

# Add a value my_param to your matrix
my_param <- 1000
writeInputTS(area = my_area,
            type = "hydroROR",
            data = ts_input_reformatted[,2:ncol(ts_input_reformatted)] + my_param,
            overwrite = TRUE,
            opts = opts
)
```

## Edit geographic trimming

``` r
# set the path to an Antares study
my_study <- file.path("", "", "")
opts <- setSimulationPath(my_study, simulation ="input")

# choose geographic trimming when creating new Antares areas
# default filtering : c("hourly","daily","weekly","monthly","annual")
initial_filtering_synthesis <- c("weekly","monthly")
initial_filtering_year_by_year <- c("monthly","annual")


opts <- createArea(name = "area1",
                   filtering = filteringOptions(
                     filter_synthesis = initial_filtering_synthesis,
                     filter_year_by_year = initial_filtering_year_by_year),
                   opts = opts)
opts <- createArea(name = "area2",opts = opts)
opts <- createLink(from = "area1", 
                   to = "area2", 
                   propertiesLink = propertiesLinkOptions(
                     filter_synthesis = initial_filtering_synthesis,
                     filter_year_by_year = initial_filtering_year_by_year),
                   opts = opts)

# check the initial filters
initial_GT <- getGeographicTrimming(areas="area1",links=TRUE,opts=opts)
print(initial_GT$areas[["area1"]])
print(initial_GT$links[["area1 - area2"]])

# edit geographic trimming of an existing area or link
new_filtering_synthesis <- c("monthly")
new_filtering_year_by_year <- c("annual")

opts <- editArea(name = "area1", 
                 filtering = list(
                   "filter_synthesis" = paste(new_filtering_synthesis,collapse = ", ")
                   "filter_year_by_year" = paste(new_filtering_year_by_year,collapse = ", ")),
                 opts = opts)
opts <- editLink(from = "area1",
                 to = "area2",
                 filter_year_by_year = new_filtering_year_by_year,
                 filter_synthesis = new_filtering_synthesis,
                 opts = opts) 

# check the new filters
new_GT <- antaresRead::getGeographicTrimming(areas="area1",links=TRUE,opts=opts)
print(new_GT$areas[["area1"]])
print(new_GT$links[["area1 - area2"]])

# important : make sure that `geographic-trimming` parameter is activated in general settings
opts <- updateGeneralSettings(geographic.trimming = TRUE,opts = opts)
```
