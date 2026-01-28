# Antares new features v9.3

``` r
library(antaresEditObject)
#> Loading required package: antaresRead
```

This article will present the new features in line with **Antares v9.3**
(cf [Antares
Simulator](https://antares-simulator.readthedocs.io/en/latest/user-guide/04-migration-guides/#v930))

## Create new study v9.3

``` r
dir_path <- tempdir()
suppressWarnings(
  createStudy(path = dir_path, 
            study_name = "test930", 
            antares_version = "9.3")
)
```

For Antares Simulator, format version is now “9.3”. For packages, we
make the adjustment and we keep “930”.

**Check version of my current study** :

``` r
current_study_opts <- simOptions()
current_study_opts$antaresVersion
#> [1] 930
```

Some minor changes to the creation of the study.

Initializes the study by updating the `generaldata.ini` file :

- The following properties were removed from settings/generaldata.ini.

  - refreshtimeseries
  - refreshintervalload
  - refreshintervalhydro
  - refreshintervalwind
  - refreshintervalthermal
  - refreshintervalsolar

## Create area

We just need create *areas* to create *st-storages*.

``` r
createArea(name = "fr")
createArea(name = "it")
```

## Short term storage

We can create new clusters, st-storage (from v8.6), with function
[`createClusterST()`](../reference/createClusterST.md). You can see
function documentation with
[`?createClusterST`](../reference/createClusterST.md).

By default you can call function only with two parameters (`area`,
`cluster_name`).

Clusters are created with default properties and time series.

### New properties

you can create or edit new clusters with new properties (see doc
[`?createClusterST`](../reference/createClusterST.md)).

``` r
# new properties (default values)
rmarkdown::paged_table(as.data.frame(storage_values_default(), check.names = FALSE))
```

``` r
# creation
my_parameters <- storage_values_default()
my_parameters$`allow-overflow` <- TRUE

createClusterST(area = "fr", 
                cluster_name = "test_storage", 
                group = "new_properties", 
                storage_parameters = my_parameters, 
                overwrite = TRUE)

createClusterST(area = "it", 
                cluster_name = "test_storage", 
                group = "new_properties", 
                storage_parameters = my_parameters,
                overwrite = TRUE)

# read cluster properties 
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)
```

``` r
# edit properties of existing st-storage cluster
my_parameters$`allow-overflow` <- FALSE

editClusterST(area = "fr", 
              cluster_name = "test_storage",
              storage_parameters = my_parameters)

# read cluster properties 
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)
```

### New dimension of **time series**

All matrices will be of (8760, N), noting that N \>= 1

``` r
# creation
ratio_value <- matrix(0.7, 8760,2)
  
# default properties with new optional TS
createClusterST(area = "fr", 
                cluster_name = "good_ts_value", 
                cost_injection = ratio_value, 
                cost_withdrawal = ratio_value, 
                cost_level = ratio_value, 
                cost_variation_injection = ratio_value,
                cost_variation_withdrawal = ratio_value)

# read cluster TS values 
tab <- readInputTS(st_storage = "all", 
                   showProgress = FALSE)
rmarkdown::paged_table(head(tab))
```

``` r
# edit TS values of existing st-storage cluster
new_ratio_value <- matrix(0.85, 8760,3)

# edit everything or anyone you want 
editClusterST(area = "fr",
              cluster_name = "good_ts_value",
              cost_injection = new_ratio_value, 
              cost_withdrawal = new_ratio_value)

# read cluster TS values 
tab <- readInputTS(st_storage = "all", 
                   showProgress = FALSE)
rmarkdown::paged_table(head(tab))
```

### Additional

All RHS will be of (8760, N), noting that N \>= 1

``` r
# Create
good_ts <- matrix(0.7, nrow = 8760, ncol =3)
createClusterST(area = "fr",
                cluster_name = "RHS_new_dimensions",
                storage_parameters = my_parameters, 
                PMAX_injection = NULL, 
                PMAX_withdrawal = NULL, 
                inflows = NULL, 
                lower_rule_curve = NULL, 
                upper_rule_curve = NULL,
                cost_injection = NULL, 
                cost_withdrawal = NULL,
                cost_level = NULL,
                cost_variation_injection = NULL, 
                cost_variation_withdrawal =NULL,
                constraints_properties = list(
                  "test"=list(
                    variable = "withdrawal",
                    operator = "equal",
                    hours = c("[1,3,5]",
                              "[120,121,122,123,124,125,126,127,128]")
                    #enabled = FALSE
                  ),
                  "test2"=list(
                    variable = "netting",
                    operator = "less",
                    hours = c("[1, 168]")
                  )),
                # constraints_ts 
                constraints_ts = list(
                  "test" = good_ts,
                  "test2"    = good_ts
                ))

# Edit
good_ts <- matrix(0.7, nrow = 8760, ncol =2)
editClusterST (area = "fr",
                cluster_name = "RHS_new_dimensions",
               constraints_ts = list(
                 "test" = good_ts,
                 "test2"    = good_ts
               )  ,
               add_prefix = TRUE)
#Read
res=read_storages_constraints()
```

    #>                       levelName
    #> 1 Root                         
    #> 2  °--fr                       
    #> 3      °--fr_rhs_new_dimensions
    #> 4          ¦--properties       
    #> 5          ¦   ¦--test         
    #> 6          ¦   °--test2        
    #> 7          °--values           
    #> 8              ¦--rhs_test     
    #> 9              °--rhs_test2

### Remove clusters

Nothing has changed to remove clusters.

``` r
# read cluster names
levels(readClusterSTDesc()$cluster)
#> [1] "fr_good_ts_value"      "fr_rhs_new_dimensions" "fr_test_storage"      
#> [4] "it_test_storage"

# remove a cluster
removeClusterST(area = "fr", 
                cluster_name = "good_ts_value")

# read cluster 
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)
```

## Generaldata

The “generaldata.ini” settings file (*settings/generaldata.ini*)
contains several sections.

Antares Simulator v9.3 deletes some parameters:

- *general/refreshtimeseries*
- *general/refreshintervalload*
- *general/refreshintervalhydro*
- *general/refreshintervalwind*
- *general/refreshintervalthermal*
- *general/refreshintervalsolar*

A message is displayed.

``` r
# user messages
updateGeneralSettings(
  refreshtimeseries = 100,
  refreshintervalload = 100,
  refreshintervalhydro = 100,
  refreshintervalwind = 100,
  refreshintervalthermal = 100,
  refreshintervalsolar = 100)
#> Warning: The `refreshtimeseries` argument of `updateGeneralSettings()` is deprecated as
#> of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `refreshintervalload` argument of `updateGeneralSettings()` is deprecated
#> as of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `refreshintervalhydro` argument of `updateGeneralSettings()` is deprecated
#> as of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `refreshintervalwind` argument of `updateGeneralSettings()` is deprecated
#> as of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `refreshintervalthermal` argument of `updateGeneralSettings()` is
#> deprecated as of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `refreshintervalsolar` argument of `updateGeneralSettings()` is deprecated
#> as of antaresEditObject 9.3.
#> ℹ This parameter is no longer supported for Antares >= 9.3; the value will be
#>   ignored and not written to generaldata.ini
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

## Scenario builder

The scenario builder allows you to use a new code `sts` for short-term
storage inflows in the following format : sts,area,year,storage = TS
number. The scenario builder allows you to use a new code `sta` for
short-term storage additional constraints in the following format :
sta,area,year,storage,constraint = TS number.

``` r
#Add sts
createClusterST(area = "fr",
                cluster_name = "Scenario_builder_sts")
 
ldata <- scenarioBuilder(
  n_scenario = 5,
  n_mc = 5,
  areas = "fr"
)
#> Warning: Specified number of Monte-Carlo years differ from the one in Antares
#> general parameter
 
updateScenarioBuilder(ldata = ldata,
                      series = "sts")

#Add sta
name_no_prefix <- "add_constraints_sta"
 
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
 
createClusterST(area = "fr",
                cluster_name = name_no_prefix,
                constraints_properties = constraints_properties)
 
ldata <- scenarioBuilder(
  n_scenario = 5,
  n_mc = 5,
  areas = "fr"
)
#> Warning: Specified number of Monte-Carlo years differ from the one in Antares
#> general parameter
 
updateScenarioBuilder(ldata = ldata,
                      series = "sta")
  
readScenarioBuilder(as_matrix = TRUE) 
#> $sta
#>                                        [,1]
#> fr_fr_add_constraints_sta_netting-1       1
#> fr_fr_add_constraints_sta_withdrawal-1    1
#> fr_fr_rhs_new_dimensions_test             1
#> fr_fr_rhs_new_dimensions_test2            1
#> 
#> $sts
#>                            [,1]
#> fr_fr_add_constraints_sta    NA
#> fr_fr_rhs_new_dimensions      1
#> fr_fr_scenario_builder_sts    1
#> fr_fr_test_storage            1
#> it_it_test_storage           NA
```

## Thematic

In settings/generaldata.ini, in section variables selection, the
following variables were removed:

- *variables selection/NUCLEAR*
- *variables selection/LIGNITE*
- *variables selection/COAL*
- *variables selection/BATTERY*
- *variables selection/GAS*
- *variables selection/OIL*
- *variables selection/MIX. FUEL*
- *variables selection/MISC. DTG*
- *variables selection/MISC. DTG 2*
- *variables selection/MISC. DTG 3*
- *variables selection/MISC. DTG 4* They’re replaced by the dynamic
  variable DISPATCH. GEN.

The following variables were removed:

- *variables selection/WIND OFFSHORE*
- *variables selection/WIND ONSHORE*
- *variables selection/SOLAR CONCRT.*
- *variables selection/SOLAR PV*
- *variables selection/SOLAR ROOFT*
- *variables selection/RENW. 1*
- *variables selection/RENW. 2*
- *variables selection/RENW. 3*
- *variables selection/RENW. 4* They’re replaced by the dynamic variable
  RENEWABLE GEN.

``` r
#List of variables version >=9.3
vector_select_vars= list_thematic_variables()
#Add all variables
list_thematic=setThematicTrimming(selection_variables = vector_select_vars$col_name[68:72])
list_thematic$parameters$`variables selection`
#> $selected_vars_reset
#> [1] FALSE
#> 
#> $`select_var +`
#> [1] "MIN DTG by plant"
#> 
#> $`select_var +`
#> [1] "STS BY GROUP"
#> 
#> $`select_var +`
#> [1] "UNSP ENRG CSR"
#> 
#> $`select_var +`
#> [1] "DISPATCH. GEN."
#> 
#> $`select_var +`
#> [1] "RENEWABLE GEN."
```
