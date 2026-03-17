# Create a short-term storage cluster

![Antares API OK](figures/badge_api_ok.svg)

Create a new ST-storage cluster for \>= v8.6.0 Antares studies.

## Usage

``` r
createClusterST(
  area,
  cluster_name,
  group = "Other1",
  storage_parameters = storage_values_default(),
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
  overwrite = FALSE,
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

- overwrite:

  `logical`, overwrite the cluster or not.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## note

To write parameters to the `list.ini` file. You have function
[`storage_values_default()`](storage_values_default.md) who is called by
default. This function return `list` containing properties according
study version for cluster `st-storage`.

Study version \>= "8.6.0" :

- efficiency = 1 (`numeric` {0;1})

- reservoircapacity = 0 (`integer` \>= 0)

- initiallevel = 0 (`numeric` {0;1})

- withdrawalnominalcapacity = 0 (`integer` \>= 0)

- injectionnominalcapacity = 0 (`integer` \>= 0)

- initialleveloptim = FALSE (`logical` TRUE/FALSE)

Study version \>= "8.8.0" (update + new parameter) :

- initiallevel = 0.5 (`numeric` {0;1})

- enabled = TRUE (`logical` TRUE/FALSE)

Study version \>= "9.2" (new parameters) :

- efficiencywithdrawal = 1 (`numeric` {0;1})

- `penalize-variation-injection` = FALSE (`logical` TRUE/FALSE)

- `penalize-variation-withdrawal` = FALSE `logical` TRUE/FALSE)

Study version \>= "9.3" (new parameters) :

- `allow-overflow` = FALSE (`logical` TRUE/FALSE)

By default, these values don't allow you to have an active cluster (See
example section.)

## See also

All the functions needed to manage a storage cluster,
[`antaresRead::readClusterSTDesc()`](https://rte-antares-rpackage.github.io/antaresRead/reference/readClusterDesc.html),
[`editClusterST()`](editClusterST.md),
[`removeClusterST()`](removeCluster.md).

## Examples

``` r
if (FALSE) { # \dontrun{

# list for cluster parameters : 
storage_values_default()

# create a cluster by default (with default parameters values + default data values):
createClusterST(area = "my_area", 
               "my_cluster") 
  
# Read cluster in study                            
 # by default, cluster name is prefixed 
 # by the area name
levels(readClusterSTDesc()$cluster)
# > "my_area_my_cluster"

# create cluster with custom parameter and data
  # use the function to create your own list of parameters (no Antares optim)
  # if you want optim (my_parameters$initialleveloptim <- TRUE)
my_parameters <- storage_values_default()
my_parameters$efficiency <- 0.5
my_parameters$initiallevel <- 10
my_parameters$withdrawalnominalcapacity <- 100
my_parameters$injectionnominalcapacity <- 1000
my_parameters$reservoircapacity <- 10000

  # time series 
inflow_data <- matrix(3, 8760)
ratio_data <- matrix(0.7, 8760)

createClusterST(area = "my_area", 
                "my_cluster",
                storage_parameters = my_parameters,
                PMAX_withdrawal = ratio_data, 
                inflows = inflow_data, 
                PMAX_injection = ratio_data, 
                lower_rule_curve = ratio_data, 
                upper_rule_curve = ratio_data)
                
# for a study version >= 9.2 (new parameters)
my_parameters <- storage_values_default()
my_parameters$efficiencywithdrawal <- 0.5
my_parameters$`penalize-variation-injection` <- TRUE
my_parameters$`penalize-variation-withdrawal` <- TRUE


createClusterST(area = "my_area", 
                "my_cluster",
                storage_parameters = my_parameters)
               
  # time series                  
ratio_value <- matrix(0.7, 8760)

# default properties with new optional TS
createClusterST(area = "fr", 
                cluster_name = "good_ts_value", 
                cost_injection = ratio_value, 
                cost_withdrawal = ratio_value, 
                cost_level = ratio_value, 
                cost_variation_injection = ratio_value,
                cost_variation_withdrawal = ratio_value)         
                
# Add optional constraints properties 
    
name_no_prefix <- "add_constraints"

constraints_properties <- list(
  "withdrawal-1"=list(
    variable = "withdrawal",
    operator = "equal",
    hours = c("[1,3,5]", 
              "[120,121,122,123,124,125,126,127,128]")
  ),
  "netting-1"=list(
    variable = "netting",
    operator = "less",
    hours = c("[1, 168]")
  ))

# create a cluster with constraint properties (no need to provide TS)
createClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_properties = constraints_properties)         

   # Add optional constraints properties + TS 
   
constraints_properties <- list(
  "withdrawal-2"=list(
    variable = "withdrawal",
    operator = "equal",
    hours = c("[1,3,5]", 
              "[120,121,122,123,124,125,126,127,128]")
  ),
  "netting-2"=list(
    variable = "netting",
    operator = "less",
    hours = c("[1, 168]")
  ))    
   
good_ts <- matrix(0.7, 8760)
constraints_ts <- list(
  "withdrawal-2"=good_ts,
  "netting-2"=good_ts)

# create a cluster with constraint properties + TS
createClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_properties = constraints_properties, 
                constraints_ts = constraints_ts)
  
# for a study version >= 9.3 (new parameters)
my_parameters <- storage_values_default()
my_parameters$`allow-overflow` <- TRUE

  # time series                  
ratio_value <- matrix(0.7, 8760, N)

# default properties with new optional TS
createClusterST(area = "fr", 
                cluster_name = "good_ts_value", 
                cost_injection = ratio_value)         
                
} # }
```
