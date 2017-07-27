# antaresEditObject


> Edit an Antares study before running simulation.


[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresEditObject.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresEditObject)



## Overview

This package provide methods to create (and remove) area, links between them, thermal cluster and binding constraints.
These steps maybe usefull before running an Antares simulation.

You can install the package with :

```r
devtools::install_github("rte-antares-rpackage/antaresEditObject"")
```



## Prerequisites

You need to set the path to an Antares simulation in "input" mode :

```r
antaresRead::setSimulationPath(path = "path/to/study", simulation = "input")
```

## Save simulation

Before modifying your simulation, you can save it in an archive :

```r
backupStudy(what = "input")
```

This will create a `.tar.gz` file in your simulation folder.



## Create a new area

You can create a new area with :

```r

library("antaresEditObject")

createArea(name = "myarea")

getOption("antares")$areaList

```


## Create a new cluster

You can initialize a cluster with some parameters :

```r
createCluster(
  area = "fr", 
  cluster_name = "myareacluster",
  group = "other",
  unitcount = 1,
  nominalcapacity = 8400,
  `min-down-time` = 0,
  `marginal-cost` = 0.010000,
  `market-bid-cost` = 0.010000,
  opts = opts
)
```


## Create a new link

```r
createLink(
  from = "fr", 
  to = "myarea", 
  propertiesLink = propertiesLinkOptions(
    hurdles_cost = FALSE,
    transmission_capacities = "enabled"
  ), 
  dataLink = NULL
)
```



## Create a binding constraint

```r
createBindingConstraint(
  name = "myconstraint", 
  values = matrix(data = c(rep(c(19200, 0, 0), each = 366)), ncol = 3), 
  enabled = FALSE, 
  timeStep = "daily",
  operator = "both",
  coefficients = c("fr%myarea" = 1)
)
```



## Remove methods

You can remove from input folder areas, links, clusters and binding constraints with `remove*` functions, e.g. :

```r
removeArea("myarea")
```


