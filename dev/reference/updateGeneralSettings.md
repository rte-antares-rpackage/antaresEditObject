# Update general parameters of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Update general parameters of an Antares study

## Usage

``` r
updateGeneralSettings(
  mode = NULL,
  horizon = NULL,
  nbyears = NULL,
  simulation.start = NULL,
  simulation.end = NULL,
  january.1st = NULL,
  first.month.in.year = NULL,
  first.weekday = NULL,
  leapyear = NULL,
  year.by.year = NULL,
  derated = NULL,
  custom.scenario = NULL,
  custom.ts.numbers = deprecated(),
  user.playlist = NULL,
  filtering = NULL,
  active.rules.scenario = NULL,
  generate = NULL,
  nbtimeseriesload = NULL,
  nbtimeserieshydro = NULL,
  nbtimeserieswind = NULL,
  nbtimeseriesthermal = NULL,
  nbtimeseriessolar = NULL,
  refreshtimeseries = NULL,
  intra.modal = NULL,
  inter.modal = NULL,
  refreshintervalload = NULL,
  refreshintervalhydro = NULL,
  refreshintervalwind = NULL,
  refreshintervalthermal = NULL,
  refreshintervalsolar = NULL,
  readonly = NULL,
  geographic.trimming = NULL,
  thematic.trimming = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- mode:

  Economy, Adequacy, Draft.

- horizon:

  Reference year (static tag, not used in the calculations)

- nbyears:

  Number of Monte-Carlo years that should be prepared for the simulation
  (not always the same as the Number of MC years actually simulated, see
  'selection mode' below).

- simulation.start:

  First day of the simulation (e.g. 8 for a simulation beginning on the
  second week of the first month of the year)

- simulation.end:

  Last day of the simulation (e.g. 28 for a simulation ending on the
  fourth week of the first month of the year)

- january.1st:

  First day of the year (Mon, Tue, etc.).

- first.month.in.year:

  Actual month by which the Time-series begin (Jan to Dec, Oct to Sep,
  etc.)

- first.weekday:

  In economy or adequacy simulations, indicates the frame (Mon- Sun,
  Sat-Fri, etc.) to use for the edition of weekly results.

- leapyear:

  (TRUE/FALSE) indicates whether February has 28 or 29 days.

- year.by.year:

  (False) No individual results will be printed out, (True) For each
  simulated year, detailed results will be printed out in an individual
  directory7 : Study_name/OUTPUT/simu_tag/Economy /mc-i-number

- derated:

  See Antares General Reference Guide.

- custom.scenario:

  See Antares General Reference Guide (see link below). Replace
  custom.ts.numbers.

- custom.ts.numbers:

  See Antares General Reference Guide (see link below). Replaced by
  custom.scenario.

- user.playlist:

  See Antares General Reference Guide (see link below).

- filtering:

  See Antares General Reference Guide (see link below).

- active.rules.scenario:

  See Antares General Reference Guide (see link below).

- generate:

  See Antares General Reference Guide (see link below).

- nbtimeseriesload:

  See Antares General Reference Guide (see link below).

- nbtimeserieshydro:

  See Antares General Reference Guide (see link below).

- nbtimeserieswind:

  See Antares General Reference Guide (see link below).

- nbtimeseriesthermal:

  See Antares General Reference Guide (see link below).

- nbtimeseriessolar:

  See Antares General Reference Guide (see link below).

- refreshtimeseries:

  See Antares General Reference Guide (see link below).

- intra.modal:

  See Antares General Reference Guide (see link below).

- inter.modal:

  See Antares General Reference Guide (see link below).

- refreshintervalload:

  See Antares General Reference Guide (see link below).

- refreshintervalhydro:

  See Antares General Reference Guide (see link below).

- refreshintervalwind:

  See Antares General Reference Guide (see link below).

- refreshintervalthermal:

  See Antares General Reference Guide (see link below).

- refreshintervalsolar:

  See Antares General Reference Guide (see link below).

- readonly:

  See Antares General Reference Guide (see link below).

- geographic.trimming:

  `logical` indicates whether to store the results for all time spans
  (FALSE) or for custom time spans (TRUE)

- thematic.trimming:

  See Antares General Reference Guide (see link below).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## See also

Antares General Reference Guide

## Examples

``` r
if (FALSE) { # \dontrun{

# Update number of Monte-Carlo years
updateGeneralSettings(nbyears = 42)

# Use a vector to update a parameter that
# can take multiple values
updateGeneralSettings(generate = c("thermal", "hydro"))

} # }
```
