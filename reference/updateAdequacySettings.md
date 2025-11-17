# Update adequacy parameters of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

Update adequacy parameters of an Antares study

## Usage

``` r
updateAdequacySettings(
  include_adq_patch = NULL,
  set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = NULL,
  set_to_null_ntc_between_physical_out_for_first_step = NULL,
  include_hurdle_cost_csr = NULL,
  check_csr_cost_function = NULL,
  enable_first_step = NULL,
  price_taking_order = NULL,
  threshold_initiate_curtailment_sharing_rule = NULL,
  threshold_display_local_matching_rule_violations = NULL,
  threshold_csr_variable_bounds_relaxation = NULL,
  opts = antaresRead::simOptions()
)
```

## Arguments

- include_adq_patch:

  Logical. If TRUE, will run Adequacy Patch

- set_to_null_ntc_from_physical_out_to_physical_in_for_first_step:

  Logical. default to TRUE

- set_to_null_ntc_between_physical_out_for_first_step:

  Logical. default to TRUE

- include_hurdle_cost_csr:

  Logical. default to FALSE

- check_csr_cost_function:

  Logical. default to FALSE

- enable_first_step:

  Logical. default to TRUE

- price_taking_order:

  Character. can take values DENS (default value) and Load.

- threshold_initiate_curtailment_sharing_rule:

  Double. default to 0.0

- threshold_display_local_matching_rule_violations:

  Double. default to 0.0

- threshold_csr_variable_bounds_relaxation:

  Integer. default to 3

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{

updateAdequacySettings(
  include_adq_patch = TRUE,
  set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = TRUE,
  set_to_null_ntc_between_physical_out_for_first_step = TRUE
)

} # }
```
