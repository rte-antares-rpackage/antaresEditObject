# Set the thematic trimming of an Antares Study

![Antares API OK](figures/badge_api_ok.svg)

Put only variables names you want to keep in study output. You can add
or remove variables (use study version \>=v8.8).

## Usage

``` r
setThematicTrimming(
  selection_variables,
  type_select = c("add", "suppr"),
  opts = simOptions()
)
```

## Arguments

- selection_variables:

  `character` of variables to add or remove.

- type_select:

  `character` select mode to add or remove (default add mode).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

You can put only variables according to version of study

## Examples

``` r
if (FALSE) { # \dontrun{

# list of variables (version >= v8.8)
vect_select_vars <- list_thematic_variables()

##
# add all variables
##
setThematicTrimming(selection_variables = vect_select_vars$variable)

##
# remove all variables
##
setThematicTrimming(selection_variables = vect_select_vars$variable, 
                    type_select = "suppr")

} # }
```
