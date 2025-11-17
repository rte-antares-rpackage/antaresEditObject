# Create a link between two areas

![Antares API OK](figures/badge_api_ok.svg)

Create a new link between two areas in an Antares study.

## Usage

``` r
createLink(
  from,
  to,
  propertiesLink = propertiesLinkOptions(),
  dataLink = NULL,
  tsLink = NULL,
  overwrite = FALSE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- from, to:

  The two areas linked together.

- propertiesLink:

  a named list containing the link properties, e.g. hurdles-cost or
  transmission-capacities for example. See
  [`propertiesLinkOptions()`](propertiesLinkOptions.md).

- dataLink:

  See Details section below.

- tsLink:

  Transmission capacities time series. First N columns are direct TS,
  following N are indirect ones.

- overwrite:

  Logical, overwrite the previous between the two areas if exist

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Details

The eight potential times-series are:

- **NTC direct** : the upstream-to-downstream capacity, in MW. Default
  to `1`.

- **NTC indirect** : the downstream-to-upstream capacity, in MW. Default
  to `1`.

- **Hurdle cost direct** : an upstream-to-downstream transmission fee,
  in euro/MWh. Default to `0`.

- **Hurdle cost indirect** : a downstream-to-upstream transmission fee,
  in euro/MWh. Default to `0`.

- **Impedances** : virtual impedances that are used in economy
  simulations to give a physical meaning to raw outputs, when no binding
  constraints have been defined to enforce Kirchhoff's laws. Default to
  `0`.

- **Loop flow** : amount of power flowing circularly though the grid
  when all "nodes" are perfectly balanced (no import and no export).
  Default to `0`.

- **PST min** : lower bound of phase-shifting that can be reached by a
  PST installed on the link, if any. Default to `0`.

- **PST max** : upper bound of phase-shifting that can be reached by a
  PST installed on the link, if any. Default to `0`.

According to Antares version, usage may vary :

**\< v7.0.0** : 5 first columns are used in the following order: NTC
direct, NTC indirect, Impedances, Hurdle cost direct, Hurdle cost
indirect.

**\>= v7.0.0** : 8 columns in order above are expected.

**\>= v8.2.0** : there's 2 cases :

- 8 columns are provided: 2 first are used in `tsLink`, other 6 are used
  for link data

- 6 columns are provided: you must provide NTC data in `tsLink`
  argument.

## Note

In Antares, areas are sorted in alphabetical order to establish links
between. For example, link between "fr" and "be" will appear under "be".
So the areas are sorted before creating the link between them, and
`dataLink` is rearranged to match the new order.

## See also

[`editLink()`](editLink.md), [`removeLink()`](removeLink.md)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)

# Set simulation path
setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")

# Create a link between two areas
createLink(from = "first_area", to  = "second_area")

} # }
```
