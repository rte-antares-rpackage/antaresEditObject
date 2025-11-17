# Create a Pumped Storage Power plant (PSP)

![Antares API OK](figures/badge_api_ok.svg)

Create a Pumped Storage Power plant (PSP)

## Usage

``` r
createPSP(
  areasAndCapacities = NULL,
  namePumping = "Psp_In",
  nameTurbining = "Psp_Out",
  hurdleCost = 5e-04,
  timeStepBindConstraint = "weekly",
  efficiency = NULL,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)

getCapacityPSP(
  area = NULL,
  nameTurbining = "Psp_Out",
  timeStepBindConstraint = "weekly",
  opts = antaresRead::simOptions()
)

editPSP(
  area = NULL,
  capacity = NULL,
  namePumping = "Psp_In",
  nameTurbining = "Psp_Out",
  timeStepBindConstraint = "weekly",
  hurdleCost = 5e-04,
  opts = antaresRead::simOptions()
)
```

## Arguments

- areasAndCapacities:

  A data.frame with 2 columns `installedCapacity` and `area`.

- namePumping:

  The name of the pumping area

- nameTurbining:

  The name of the turbining area

- hurdleCost:

  The cost of the PSP

- timeStepBindConstraint:

  Time step for the binding constraint : `daily` or `weekly`

- efficiency:

  The efficiency of the PSP

- overwrite:

  Overwrite the Pumped Storage Power plant if already exist. This will
  overwrite the previous area and links.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- area:

  an area name

- capacity:

  PSP capacity for the area

## Value

An updated list containing various information about the simulation.

`getCapacityPSP()` returns PSP capacity of the area

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresEditObject)
path<-pathToYourStudy
opts<-setSimulationPath(path, simulation = "input")
pspData<-data.frame(area=c("a", "b"), installedCapacity=c(800,900))

createPSP(pspData, efficiency = 0.8)

createPSP(pspData, efficiency = 0.66, overwrite = TRUE)
createPSP(pspData, efficiency = 0.98, timeStepBindConstraint = "daily")
getAreas()

} # }

if (FALSE) { # \dontrun{

getCapacityPSP("a")
editPSP("a", capacity = 8000, hurdleCost = 0.1)
getCapacityPSP("a")

areaName<-"suisse"
createArea(areaName, overwrite = TRUE)
pspData<-data.frame(area=c(areaName), installedCapacity=c(9856))
createPSP(pspData, efficiency = 0.5, overwrite = TRUE, timeStepBindConstraint = "daily")

getCapacityPSP(areaName, timeStepBindConstraint = "daily")


} # }
```
