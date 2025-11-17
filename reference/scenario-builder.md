# Read, create, update & deduplicate scenario builder

![Antares API OK](figures/badge_api_ok.svg)

Read, create, update & deduplicate scenario builder.

## Usage

``` r
scenarioBuilder(
  n_scenario = 1,
  n_mc = NULL,
  areas = NULL,
  areas_rand = NULL,
  group_bc = NULL,
  group_bc_rand = NULL,
  coef_hydro_levels = NULL,
  mode = NULL,
  opts = antaresRead::simOptions()
)

readScenarioBuilder(
  ruleset = "Default Ruleset",
  as_matrix = TRUE,
  opts = antaresRead::simOptions()
)

updateScenarioBuilder(
  ldata,
  ruleset = "Default Ruleset",
  series = NULL,
  clusters_areas = NULL,
  links = NULL,
  opts = antaresRead::simOptions()
)

clearScenarioBuilder(
  ruleset = "Default Ruleset",
  opts = antaresRead::simOptions()
)

deduplicateScenarioBuilder(
  ruleset = "Default Ruleset",
  opts = antaresRead::simOptions()
)
```

## Arguments

- n_scenario:

  Number of scenario.

- n_mc:

  Number of Monte-Carlo years.

- areas:

  Areas to use in scenario builder, if `NULL` (default) all areas in
  Antares study are used.

- areas_rand:

  Areas for which to use `"rand"`.

- group_bc:

  `character` Bindgind constraints's groups names to use.

- group_bc_rand:

  `character` Bindgind constraints which to use `"rand"`.

- coef_hydro_levels:

  Hydro levels or hydro final level coefficients.

- mode:

  `character` "bc" to edit binding constraints.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- ruleset:

  Ruleset to read.

- as_matrix:

  If `TRUE` (default) return a `matrix`, else a `list`.

- ldata:

  A `matrix` obtained with `scenarioBuilder`, or a named list of
  matrices obtained with `scenarioBuilder`, names must be 'l', 'h', 'w',
  's', 't', 'r', 'ntc', 'hl', 'bc', 'hfl', 'sts' or 'sta', depending on
  the series to update.

- series:

  Name(s) of the serie(s) to update if `ldata` is a single `matrix`.

- clusters_areas:

  A `data.table` with two columns `area` and `cluster` to identify
  area/cluster couple to update for thermal or renewable series. Default
  is to read clusters description and update all couples area/cluster.

- links:

  Links to use if series is `"ntc"`. Either a simple vector with links
  described as `"area01%area02` or a `data.table` with two columns
  `from` and `to`. Default is to read existing links and update them
  all.

## Value

`scenarioBuilder` : a `matrix`

`readScenarioBuilder` : a `list` of `matrix` or `list` according to
`as_matrix` parameters.

## Note

- `series = "ntc"` is only available with Antares \>= 8.2.0.

- For `series = "hl/hfl"`, each value must be between 0 and 1.

- User must enable/disable `custom-scenario` property in
  `settings/generaldata.ini` by himself.

- `series = "bc"` is only available with Antares \>= 8.7.0.

For a single matrix, value of series can be :

- h or hydro

- hl or hydrolevels

- l or load

- ntc

- r or renewables

- s or solar

- t or thermal

- w or wind

- hfl or hydro final level

- sts or sct apports

- sta or sct contraintes

## See also

[Scenario Builder
vignette](https://rte-antares-rpackage.github.io/antaresEditObject/articles/scenario-builder.html)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)
library(antaresEditObject)

# simulation path
setSimulationPath(
  path = "pat/to/simulation",
  simulation = "input"
)

# Create a scenario builder matrix
sbuilder <- scenarioBuilder(
  n_scenario = 51,
  n_mc = 2040,
  areas_rand = c("fr", "be")
)
sbuilder[, 1:6]
dim(sbuilder)

# Create a scenario builder matrix for hydro levels (use case 1)
sbuilder <- scenarioBuilder(
  n_mc = opts$parameters$general$nbyears,
  areas = c("fr", "be"),
  coef_hydro_levels = c(0.1, 0.9)
)

# Create a scenario builder matrix for hydro levels (use case 2)
sbuilder <- scenarioBuilder(
  n_mc = opts$parameters$general$nbyears,
  areas = c("fr", "be"),
  coef_hydro_levels = c(runif(opts$parameters$general$nbyears)
  , runif(opts$parameters$general$nbyears)
  )
)

# Create a scenario builder matrix with 
 # bindings constraints groups (study version >= 8.7.0)
  # Use parameter "mode" with "bc"   
sbuilder <- scenarioBuilder(
  n_scenario = 51,
  n_mc = 2040,
  group_bc = c("my_bc_1", "my_bc_2"), 
  group_bc_rand = "my_bc_2",
  mode = "bc"
)

# Read previous scenario builder
# in a matrix format
prev_sb <- readScenarioBuilder()


# Update scenario builder

# Single matrix for load serie
updateScenarioBuilder(ldata = sbuilder, series = "load") # can be l instead of load

# equivalent as
updateScenarioBuilder(ldata = list(l = sbuilder))

# for binding constraints (study version >= 8.7.0)
updateScenarioBuilder(ldata = sbuilder, series = "bc")

# update several series

# same input
sbuilder
updateScenarioBuilder(
  ldata = sbuilder, 
  series = c("load", "hydro", "solar")
)

# List of matrix
updateScenarioBuilder(ldata = list(
  l = load_sb,
  h = hydro_sb,
  s = solar_sb
))
# for binding constraints (study version >= 9.3.0)
updateScenarioBuilder(ldata = sbuilder, series = "sts")
updateScenarioBuilder(ldata = sbuilder, series = "sta")

# Deduplicate scenario builder

deduplicateScenarioBuilder()
} # }
```
