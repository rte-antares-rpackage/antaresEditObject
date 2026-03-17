# Antares new features v9.2

``` r
library(antaresEditObject)
#> Loading required package: antaresRead
```

This article will present the new features in line with **Antares v9.2**
(cf [Antares
Simulator](https://antares-simulator.readthedocs.io/en/latest/user-guide/04-migration-guides/#v920))

## Create new study v9.2

``` r
dir_path <- tempdir()
suppressWarnings(
  createStudy(path = dir_path, 
            study_name = "test920", 
            antares_version = "9.2")
)
```

For Antares Simulator, format version is now “9.2”. For packages, we
make the adjustment and we keep “920”.

**Check version of my current study** :

``` r
current_study_opts <- simOptions()
current_study_opts$antaresVersion
#> [1] 920
```

Some minor changes to the creation of the study.

Initializes the study by updating the `generaldata.ini` file :

- the value of the `shedding-policy` parameter is changed to “accurate
  shave peaks”
- a new “compatibility” section is created with parameter `hydro-pmax` =
  “daily”

## Create area

We just need create *areas* to create *st-storages*.

``` r
createArea(name = "fr")
createArea(name = "it")
```

A new parameter `overflow spilled cost difference` in `hydro.ini` file
(*input/hydro/hydro.ini*) is initiated (*default value = 1*).

## Short term storage

We can create new clusters, st-storage (from v8.6), with function
[`createClusterST()`](../reference/createClusterST.md). You can see
function documentation with
[`?createClusterST`](../reference/createClusterST.md).

By default you can call function only with two parameters (`area`,
`cluster_name`).

Clusters are created with default properties and time series.

### Dynamic group names

Default `group` is still “Other1” and now you can **create/edit** your
own group name (*only for version study \>= 9.2*).

``` r
# creation
createClusterST(area = "fr", 
                cluster_name = "test_storage", 
                group = "my_own_group")

createClusterST(area = "it", 
                cluster_name = "test_storage", 
                group = "my_own_group_again")

# edit group of existing st-storage cluster
editClusterST(area = "fr", 
              cluster_name = "test_storage", 
              group = "my_own_group_Pondage")

# read cluster properties
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)
```

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
my_parameters$efficiencywithdrawal <- 0.5
my_parameters$`penalize-variation-injection` <- TRUE
my_parameters$`penalize-variation-withdrawal` <- TRUE

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
my_parameters$efficiencywithdrawal <- 0.9
my_parameters$`penalize-variation-injection` <- FALSE
my_parameters$`penalize-variation-withdrawal` <- FALSE

editClusterST(area = "fr", 
              cluster_name = "test_storage",
              storage_parameters = my_parameters)

# read cluster properties 
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)
```

### New optional **time series**

We have five new *.txt* files containing one series of dimension
$N = 8760,P = 1$ :

- cost-injection.txt  
- cost-withdrawal.txt
- cost-level.txt  
- cost-variation-injection.txt  
- cost-variation-withdrawal.txt

``` r
# creation
ratio_value <- matrix(0.7, 8760)
  
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
new_ratio_value <- matrix(0.85, 8760)

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

We can add additional constraints via the dedicated endpoints:

- constraints_properties
- TS constraints rhs\_\*

The function read_storages_constraints() reads all short-term storage
constraints from API or disk and returns them as a nested R list. Each
top-level element corresponds to an area. Inside each area you will
find, for each storage cluster, two main sub-lists:

properties – general information and constraint definitions, such as:

- variable (e.g. “withdrawal”)
- operator (e.g. “equal”)
- hours (e.g. “\[1, 3, 5\]”)
- enabled (logical flag)

values - time-series data linked to those constraints, for example a
rhs_test element containing a numeric vector of hourly values (length
8760).

#### Properties

``` r
# Create 
createClusterST(area = "fr",
                cluster_name = "Additional_Properties",
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
                    hours = c("[1,3,5]"),
                  "test2"=list(
                   variable = "netting",
                   operator = "less",
                   hours = c("[1, 168]")
                 )
                  )))

# Edit 
editClusterST (area = "fr", 
               cluster_name = "Additional_Properties", 
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
               constraints_properties <- list(
                 "test"=list(
                   variable = "withdrawal",
                   operator = "equal",
                   hours = c("[1,3,5]",
                              "[120,121,122,123,124,125,126,127,128]"),
                   enabled = FALSE
                 ),
                 "test2"=list(
                   variable = "netting",
                   operator = "less",
                   hours = c("[1, 168]")
                 )))
```

#### Values

``` r
# Create
good_ts <- matrix(0.7, nrow = 8760, ncol = 1)
createClusterST(area = "fr",
                cluster_name = "Additional_Values",
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
editClusterST (area = "fr",
                cluster_name = "Additional_Values",
               constraints_ts = list(
                 "test" = good_ts,
                 "test2"    = good_ts+1
               )  ,
               add_prefix = TRUE)
#Read
res=read_storages_constraints()
```

    #>                           levelName
    #> 1  Root                            
    #> 2   °--fr                          
    #> 3       ¦--fr_additional_properties
    #> 4       ¦   ¦--properties          
    #> 5       ¦   ¦   °--test            
    #> 6       ¦   °--values              
    #> 7       °--fr_additional_values    
    #> 8           ¦--properties          
    #> 9           ¦   ¦--test            
    #> 10          ¦   °--test2           
    #> 11          °--values              
    #> 12              ¦--rhs_test        
    #> 13              °--rhs_test2

### Remove clusters

Nothing has changed to remove clusters.

``` r
# read cluster names
levels(readClusterSTDesc()$cluster)
#> [1] "fr_additional_properties" "fr_additional_values"    
#> [3] "fr_good_ts_value"         "fr_test_storage"         
#> [5] "it_test_storage"

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

Antares Simulator v9.2 deletes some parameters:

- *adequacy patch/enable-first-step*
- *adequacy patch/set-to-null-ntc-between-physical-out-for-first-step*
- *other preferences/initial-reservoir-levels*

A message is displayed and parameters are set to `NULL` for the
`[adequacy patch]` section.

For the `[other preferences]` section, the `initial-reservoir-levels`
parameter is not explicitly used by a dedicated function.

``` r
# user messages
updateAdequacySettings(
    set_to_null_ntc_between_physical_out_for_first_step = FALSE)
#> Warning: The `set_to_null_ntc_between_physical_out_for_first_step` argument of
#> `updateAdequacySettings()` is deprecated as of antaresEditObject 2.9.2.
#> ℹ This parameter are no longer supported for an Antares version >= '9.2', the
#>   values will be ignored.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
updateAdequacySettings(enable_first_step = FALSE)
#> Warning: The `enable_first_step` argument of `updateAdequacySettings()` is deprecated as
#> of antaresEditObject 2.9.2.
#> ℹ This parameter are no longer supported for an Antares version >= '9.2', the
#>   values will be ignored.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

## Scenario builder

The scenario builder allows you to use a new code `hfl` for ‘hydro final
level’. This new feature is similar to `hl` (‘hydro levels’) and is used
in the same way with the ‘coef_hydro_levels’ parameter.

``` r
# the number of coeff is equivalent to the number of areas
  my_coef <- runif(length(getAreas()))
  
  opts <- simOptions()
  
  # build data 
  ldata <- scenarioBuilder(
    n_scenario = 10,
    n_mc = 10,
    areas = getAreas(),
    coef_hydro_levels = my_coef
  )
#> Warning: Specified number of Monte-Carlo years differ from the one in Antares
#> general parameter
  
  # update scenearionbuilder.dat
  updateScenarioBuilder(ldata = ldata,
                        series = "hfl")
  
  readScenarioBuilder(as_matrix = TRUE)
#> $hfl
#>          [,1]
#> fr 0.08075014
#> it 0.83433304
```
