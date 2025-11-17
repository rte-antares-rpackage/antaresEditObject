# Create a Demand Side Response (DSR)

![Antares API OK](figures/badge_api_ok.svg)

Create a Demand Side Response (DSR)

## Usage

``` r
createDSR(
  areasAndDSRParam = NULL,
  spinning = 2,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)

getCapacityDSR(area = NULL, opts = antaresRead::simOptions())

editDSR(
  area = NULL,
  unit = NULL,
  nominalCapacity = NULL,
  marginalCost = NULL,
  spinning = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- areasAndDSRParam:

  A `data.frame` with 4 columns `area`, `unit`, `nominalCapacity`,
  `marginalCost` and `hour`. Hour represent the number of activation
  hours for the DSR per day.

- spinning:

  DSR spinning

- overwrite:

  Overwrite the DSR plant if already exist. This will overwrite the
  previous area and links.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- area:

  an area where to edit the DSR

- unit:

  DSR unit number

- nominalCapacity:

  DSR nominalCapacity

- marginalCost:

  DSR marginalCost

## Value

An updated list containing various information about the simulation.

`getCapacityDSR()` returns DSR capacity (unit \* nominalCapacity of
virtual cluster) of the area

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresEditObject)
path <- pathToYourStudy
opts <- setSimulationPath(path, simulation = "input")

# area, unit, nominalCapacity and marginalCost
dsrData <- data.frame(area = c("a", "b"), unit = c(10,20), 
                    nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))

createDSR(dsrData)

createDSR(dsrData, spinning = 3, overwrite = TRUE)
getAreas()

} # }
if (FALSE) { # \dontrun{

getCapacityDSR("a")
editDSR("a", unit = 50, nominalCapacity = 8000)
getCapacityDSR("a")

} # }
if (FALSE) { # \dontrun{

getCapacityDSR("a")
editDSR("a", unit = 50, nominalCapacity = 8000, marginalCost = 45, hour = 9)
getCapacityDSR("a")

} # }
```
