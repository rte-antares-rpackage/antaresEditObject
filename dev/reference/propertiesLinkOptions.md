# Properties for creating a link

Properties for creating a link

## Usage

``` r
propertiesLinkOptions(
  hurdles_cost = FALSE,
  transmission_capacities = "enabled",
  asset_type = "ac",
  display_comments = TRUE,
  filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
  filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual"),
  use_phase_shifter = FALSE,
  loop_flow = FALSE,
  colorr = 112,
  colorb = 112,
  colorg = 112,
  link_width = 1,
  link_style = "plain"
)
```

## Arguments

- hurdles_cost:

  Logical, which is used to state whether (linear) transmission fees
  should be taken into account or not in economy and adequacy
  simulations

- transmission_capacities:

  Character, one of `enabled`, `ignore` or `infinite`, which is used to
  state whether the capacities to consider are those indicated in
  8760-hour arrays or if zero or infinite values should be used instead
  (actual values / set to zero / set to infinite)

- asset_type:

  Character, one of `ac`, `dc`, `gas`, `virt` or `other`. Used to state
  whether the link is either an AC component (subject to Kirchhoff’s
  laws), a DC component, or another type of asset.

- display_comments:

  Logical, display comments or not.

- filter_synthesis:

  Character, vector of time steps used in the output synthesis, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

- filter_year_by_year:

  Character, vector of time steps used in the output year-by-year, among
  `hourly`, `daily`, `weekly`, `monthly`, and `annual`

- use_phase_shifter:

  Logical.

- loop_flow:

  Logical.

- colorr:

  Integer, color of the line.

- colorb:

  Integer, color of the line.

- colorg:

  Integer, color of the line.

- link_width:

  Numeric, width of the line.

- link_style:

  Character, style of the line.

## Value

A named list that can be used in [`createLink()`](createLink.md).

## Examples

``` r
if (FALSE) { # \dontrun{
propertiesLinkOptions(
  hurdles_cost = TRUE,
  filter_synthesis=c("hourly","daily"),
  filter_year_by_year=c("weekly","monthly")
)
} # }
```
