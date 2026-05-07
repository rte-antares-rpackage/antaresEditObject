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

##
# Update a list of variables
##

opts <- setSimulationPath(path = path_study, simulation = "input")

thematic <- antaresRead::getThematicTrimming(opts = opts)
active <- thematic[thematic$status_selection == "active",]$variables

# Add new variables to active variables
to_add <- c("var1", "var2", "var3")
new_list <- union(active, to_add)
opts <- setThematicTrimming(selection_variables = new_list, type_select = "add", opts = opts)

# Remove existing variables from active variables
to_remove <- c("var1", "var2", "var3")
new_list <- setdiff(active, to_remove)
opts <- setThematicTrimming(selection_variables = new_list, type_select = "add", opts = opts)
} # }
```
