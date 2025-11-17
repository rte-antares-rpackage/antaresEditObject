# Edit a short-term storage cluster

![Antares API OK](figures/badge_api_ok.svg)

Edit parameters and time series of an existing `st-storage` cluster
(Antares studies \>= v8.6.0).

## Usage

``` r
editClusterST(
  area,
  cluster_name,
  group = NULL,
  storage_parameters = NULL,
  PMAX_injection = NULL,
  PMAX_withdrawal = NULL,
  inflows = NULL,
  lower_rule_curve = NULL,
  upper_rule_curve = NULL,
  cost_injection = NULL,
  cost_withdrawal = NULL,
  cost_level = NULL,
  cost_variation_injection = NULL,
  cost_variation_withdrawal = NULL,
  constraints_properties = NULL,
  constraints_ts = NULL,
  add_prefix = TRUE,
  opts = antaresRead::simOptions()
)
```

## Arguments

- area:

  The area where to create the cluster.

- cluster_name:

  Name for the cluster, it will prefixed by area name, unless you set
  `add_prefix = FALSE`.

- group:

  Group of the cluster, one of : *PSP_open, PSP_closed, Pondage,
  Battery, Other*. It corresponds to the type of stockage (**dynamic
  name for Antares version \>= 9.2**).

- storage_parameters:

  `list ` Parameters to write in the Ini file (see `Note`).

- PMAX_injection:

  Modulation of charging capacity on an 8760-hour basis. `numeric` {0;1}
  (8760*1) (\*\*`numeric` {0;1} (8760*N) noting that N \>= 1 for Antares
  version \>= 9.3\*\*).

- PMAX_withdrawal:

  Modulation of discharging capacity on an 8760-hour basis. `numeric`
  {0;1} (8760*1) (\*\*`numeric` {0;1} (8760*N) noting that N \>= 1 for
  Antares version \>= 9.3\*\*).

- inflows:

  Algebraic deviation of the state of charge of the storage, which does
  not induce any power generation or consumption on the system `numeric`
  {\<0;\>0} (8760*1) (\*\*“numeric\` {\<0;\>0} (8760*1) noting that N
  \>= 1 for Antares version \>= 9.3\*\*).

- lower_rule_curve:

  This is the lower limit for filling the stock imposed each hour.
  `numeric` {0;1} (8760*1) (\*\*`numeric` {0;1} (8760*N) noting that N
  \>= 1 for Antares version \>= 9.3\*\*).

- upper_rule_curve:

  This is the upper limit for filling the stock imposed each hour.
  `numeric` {0;1} (8760*1) (\*\*`numeric` {0;1} (8760*N) noting that N
  \>= 1 for Antares version \>= 9.3\*\*).

- cost_injection:

  Penalizes the injection flowrate at each hour (€/MWh) `numeric` {\>0}
  (8760*1) (\*\*`numeric` {\>0} (8760*N) noting that N \>= 1 for Antares
  version \>= 9.3\*\*).

- cost_withdrawal:

  Penalizes the withdrawal flowrate at each hour (€/MWh) `numeric` {\>0}
  (8760*1) (\*\*`numeric` {\>0} (8760*N) noting that N \>= 1 for Antares
  version \>= 9.3\*\*).

- cost_level:

  Penalizes the volume of stored energy at each hour (€/MWh) `numeric`
  {\<0;\>0} (8760*1) (\*\*`numeric` {\<0;\>0} (8760*N) noting that N \>=
  1 for Antares version \>= 9.3\*\*).

- cost_variation_injection:

  Penalizes injection flowrate variation every hour (€/MWh) `numeric`
  {\>0} (8760*1) (\*\*`numeric` {\>0} (8760*N) noting that N \>= 1 for
  Antares version \>= 9.3\*\*).

- cost_variation_withdrawal:

  Penalizes the withdrawal variation every hour (€/MWh) `numeric` {\>0}
  (8760*1) (\*\*`numeric` {\>0} (8760*N) noting that N \>= 1 for Antares
  version \>= 9.3\*\*).

- constraints_properties:

  `list ` Parameters (see example)

- constraints_ts:

  `list ` of time series (see example)

- add_prefix:

  If `TRUE` (the default), `cluster_name` will be prefixed by area name.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Note

Put only properties or TS value you want to edit (see `examples`
section).

## See also

[`createClusterST()`](createClusterST.md),
[`removeClusterST()`](removeCluster.md)

## Examples
