# Update an existing binding constraint

![Antares API OK](figures/badge_api_ok.svg)**\[experimental\]**

Update an existing binding constraint in an Antares study. The key
search value of the constraint is the `id` field

## Usage

``` r
editBindingConstraint(
  name,
  id = tolower(name),
  values = NULL,
  enabled = NULL,
  timeStep = NULL,
  operator = NULL,
  filter_year_by_year = NULL,
  filter_synthesis = NULL,
  coefficients = NULL,
  group = NULL,
  opts = simOptions()
)
```

## Arguments

- name:

  The name for the binding constraint.

- id:

  An id, default is to use the name.

- values:

  Values used by the constraint. It contains one line per time step and
  three columns "less", "greater" and "equal" (see documentation below
  if you're using version study \>= v8.7.0)

- enabled:

  Logical, is the constraint enabled ?

- timeStep:

  Time step the constraint applies to : `hourly`, `daily` or `weekly`.

- operator:

  Type of constraint: equality, inequality on one side or both sides.

- filter_year_by_year:

  Marginal price granularity for year by year

- filter_synthesis:

  Marginal price granularity for synthesis

- coefficients:

  A named list containing the coefficients used by the constraint, the
  coefficients have to be alphabetically ordered see examples below for
  entering weight or weight with offset.

- group:

  "character" group of the constraint, default value : "default"

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Warning

Put values with rights dimensions :

- hourly : 8784

- daily = 366

**\>= v8.7.0** : For each constraint name, one file .txt containing
`<id>_lt.txt, <id>_gt.txt, <id>_eq.txt` Parameter `values` must be named
`list` ("lt", "gt", "eq") containing `data.frame` scenarized. see
example section below.

## See also

Other binding constraints functions:
[`createBindingConstraintBulk()`](createBindingConstraintBulk.md),
[`createBindingConstraint()`](createBindingConstraint.md),
[`removeBindingConstraint()`](removeBindingConstraint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 # < v8.7.0 :
editBindingConstraint(
  name = "myconstraint", 
  values = matrix(data = rep(0, 8784 * 3), ncol = 3), 
  enabled = FALSE, 
  timeStep = "hourly",
  operator = "both",
  coefficients = list("fr%de" = 1)
)

# update binding constraint with weight + offset 
editBindingConstraint(
  name = "myconstraint", 
  values = matrix(data = rep(0, 8784 * 3), ncol = 3), 
  enabled = FALSE, 
  timeStep = "hourly",
  operator = "both",
  coefficients = list("fr%de" = "1%-5")
)

 # >= v8.7.0 :
 
# data values scenarized (hourly)
df <- matrix(data = rep(0, 8784 * 3), ncol = 3)
 
# you can provide list data with all value 
# or just according with 'operator' (ex : 'lt' for 'less)
values_data <- list(lt=df, 
                   gt= df, 
                   eq= df)  
                     
editBindingConstraint(name = "myconstraint", 
                      values = values_data, 
                      enabled = TRUE, 
                      timeStep = "hourly", 
                      operator = "both", 
                      filter_year_by_year = "hourly", 
                      filter_synthesis = "hourly", 
                      coefficients = list("fr%de" = 1), 
                      group = "myconstraint_group")                   
} # }
```
