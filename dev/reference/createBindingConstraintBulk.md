# Create multiple binding constraint at once.

**\[experimental\]** ![Antares API NO](figures/badge_api_no.svg)

## Usage

``` r
createBindingConstraintBulk(constraints, opts = antaresRead::simOptions())
```

## Arguments

- constraints:

  A `list` of several named `list` containing data to create binding
  constraints. **Warning** all arguments for creating a binding
  constraints must be provided, see examples.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Details

According to Antares version, usage may vary :

**\>= v8.7.0** :

- For each constraint name, one file .txt containing
  `<id>_lt.txt, <id>_gt.txt, <id>_eq.txt`.

- Parameter `values` must be named `list` ("lt", "gt", "eq") containing
  `data.frame` scenarized.

- Add parameter `group` in input list `constraints`

see example section below.

## See also

Other binding constraints functions:
[`createBindingConstraint()`](createBindingConstraint.md),
[`editBindingConstraint()`](editBindingConstraint.md),
[`removeBindingConstraint()`](removeBindingConstraint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# For Study version < v8.7.0
# Create multiple constraints

# Prepare data for constraints 
bindings_constraints <- lapply(
  X = seq_len(100),
  FUN = function(i) {
    # use arguments of createBindingConstraint()
    # all arguments must be provided !
    list(
      name = paste0("constraints", i), 
      id = paste0("constraints", i), 
      values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("area1%area2" = 1),
      overwrite = TRUE
    )
  }
)
# create all constraints
createBindingConstraintBulk(bindings_constraints)

# For Study version >= v8.7.0 (add parameter `group`)

# data values (hourly)
df <- matrix(data = rep(0, 8760 * 3), ncol = 3)
values_data <- list(lt=df, 
                    gt= df)   

# create multiple constraints
bindings_constraints <- lapply(
  X = seq_len(10),
  FUN = function(i) {
    # use arguments of createBindingConstraint()
    # all arguments must be provided !
    list(
      name = paste0("constraints_bulk", i), 
      id = paste0("constraints_bulk", i), 
      values = values_data, 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("at%fr" = 1),
      group= "group_bulk",
     overwrite = TRUE
   )
  }
)
 
createBindingConstraintBulk(bindings_constraints)  
} # }
```
