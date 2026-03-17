# Remove a Binding Constraint

![Antares API OK](figures/badge_api_ok.svg)**\[experimental\]**

Remove a binding constraint in an Antares study.

## Usage

``` r
removeBindingConstraint(
  name = NULL,
  group = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- name:

  Name(s) of the binding constraint(s) to remove.

- group:

  `character` Name(s) of group to delete

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

Starting with version **v8.7.0**, you can delete binding constraints by
name or by group.

## See also

Other binding constraints functions:
[`createBindingConstraintBulk()`](createBindingConstraintBulk.md),
[`createBindingConstraint()`](createBindingConstraint.md),
[`editBindingConstraint()`](editBindingConstraint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# < v8.7.0 :
removeBindingConstraint(name = "mybindingconstraint")

# >= v8.7.0 (delete by names group) :
# read
bc <- readBindingConstraints()

# select all groups
group_to_delete <- sapply(bc, function(x){
  x$properties$group
})

# delete all groups
removeBindingConstraint(group = group_to_delete)
} # }
```
