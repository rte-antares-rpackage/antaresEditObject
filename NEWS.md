> Copyright © 2016 RTE Reseau de transport d’electricite

# antaresEditObject 0.9.2.9000
(cf. Antares v9.2 changelog)

NEW FEATURES :  

* `createStudy()` initializes the study by updating the `generaldata.ini` file :  
  - the value of the `shedding-policy` parameter is changed to "accurate shave peaks"
  - a new "compatibility" section is created with parameter `hydro-pmax` = "daily"
* `createArea()` initializes the `hydro.ini` file with a new parameter `overflow spilled cost difference` (for each area)
* `createClusterST()`/`editClusterST()` : Parameter `group` is now dynamic and have no restriction 
* `createClusterST()`/`editClusterST()` :  
  - **New properties** (*efficiencywithdrawal*, *penalize-variation-injection*, *penalize-variation-withdrawal*, see list of properties according to study version of Antares with `storage_values_default()`)  
  - **New optional time series** (cost-injection, cost-withdrawal, cost-level, cost-variation-injection, cost-variation-withdrawal)
  - **New additional constraints** (properties and time series)  
* `removeClusterST()` : remove **New optional time series** + **New additional constraints**    
* `updateScenarioBuilder()` New type of series "hfl" ("hydro final level", similar to "hydrolevels") is available
* `removeDistrict()` remove a district from a study
* `createArea()`/`editArea()`: in API mode, allow the user to customize localization and color of an area


NEW FEATURES (other) :  

* `editBindingConstraint()` : control the dimensions of the matrix only if a time series is provided by the user for optimization
* `.createCluster()` uses a specific endpoint to write cluster's metadata and commands to write matrix
* Add new function `setThematicTrimming()` to set the thematic trimming in file `generaldata.ini`


### Breaking changes  :  
  - `createClusterST()` : For a study < *v9.2*, execution will be STOP if `group` is not included in list (see doc)  
  - `updateAdequacySettings()` : Two parameters (*enable-first-step*, *set-to-null-ntc-between-physical-out-for-first-step*) are **deprecated** and removed. Parameters are forced to `NULL` with study >= v9.2.
    

### DOC :  

A new article exposing new features of Antares Simulator v9.2 is available [here](https://rte-antares-rpackage.github.io/antaresEditObject/dev/articles/Antares_new_features_v920.html)

  


# antaresEditObject 0.9.0
(cf. Antares v9 changelog)

NEW FEATURES (Antares v9.0) :  

* `createStudy()` takes into account the new format of Antares studies (e.g. 9.0, 9.15 instead of 900, 915)  

BUGFIXES :  

* `editBindingConstraint()` :  
  - `operator` parameter set to NULL, by default, no longer causes an error.  
  - To add values, the `operator` parameter is now required.  
  - For a study version >= 832, the `filter-year-by-year` and `filter-synthesis` properties are retained in the .ini file if they are not modified.  
* *[private function]* `api_command_execute()` manage snapshot generation of a variant study with a tempo to wait the end of current task (prevents the order from being ignored).  
  - You can use global parameter `verbose` to `TRUE` ([#274](https://github.com/rte-antares-rpackage/antaresRead/pull/274) `antaresRead`) to display diagnostic messages (`getOption("antares")`)
* `updateAdequacySettings()` : in API mode do not send NULL value


GITHUB ACTIONS :  

* Actions artifacts v3 is closing down, update to v4  
* test-coverage.yaml updated 


# antaresEditObject 0.7.1

### Breaking changes  :  

* `createBindingConstraint()` / `editBindingConstraint()` uses metadata to check the group size of time series.
* `createBindingConstraintBulk()` checks consistency of groups passed as parameters and consistency with the study.
* `importZipStudyWeb()` can delete the zipfile and move the study in Antares Web to another folder
* delete `antaresRead-reexports.R` and adjust scripts to have a clean package
* `removeArea()` : send a warning instead of a stop if an area is referenced in a binding constraint coefficient
* `removeLink()` : send a warning instead of a stop if a link is referenced in a binding constraint coefficient
* `removeCluster()` : send a warning instead of a stop if a cluster is referenced in a binding constraint coefficient
* `createClusterST()` : updated with new endpoint API (POST + PUT)
* `editClusterST()` : updated with new endpoint API (PATCH + PUT)
* `removeCluster()`/`removeClusterRES()`/`removeClusterST()` updated with new endpoint API (DELETE)

NEW FEATURES (Antares v8.8) :

* `updateOptimizationSettings()` allows the user to update solver.log property  
* `createClusterST()` / `editClusterST()` use new parameters and default values
* Add new function `api_patch()` to put PATCH (httr) request to API 


BUGFIXES :  

* `createBindingConstraint()` in API mode (for study <v870) created with "hourly" timeStep all the time
* `createBindingConstraint()` / `editBindingConstraint()` in TEXT mode, bad values in time series  
* `createBindingConstraintBulk()` with no VALUES and with a mix 
* Enable control of matrix dimension in `.check_bulk_object_dim()` even if the values are not in first position in the list
* `editLink()` : avoid *NULL* value (default) for arguments *filter_synthesis* and *filter_year_by_year* to write an empty string
* `updateOutputSettings()` : in API mode, allow the user to edit the desired property
* `setPlaylist()`: do not send NULL value if the weights are not provided in argument


OTHER UPDATES :  

* `updateGeneralSettings()` : replace custom.ts.numbers argument by custom.scenario and deprecate custom.ts.numbers  
* `updateGeneralSettings()` : add thematic.trimming argument for edition  

DOC :  

* `createClusterST()` : update doc to discrabe st-storage list parameters + "Inflows" parameter

# antaresEditObject 0.7.0 

> Scenarized RHS for binding constraints

NEW FEATURES (Antares v8.7, cf. Antares v8.7 changelog) :  

* `createBindingConstraint()` / `createBindingConstraintBulk()` 
  - New parameters `group`  
  - Parameter `values` is now list of `data.frame` 
  
* `editBindingConstraint()` 
  - New parameters `group`  
  - Parameter `values` is now list of `data.frame`
  
* `removeBindingConstraint()` can now delete coupling constraints from the `group` parameter.
* `scenarioBuilder()` has 3 new parameters dedicated to use with binding constraints.
* `updateGeneralSettings()` adds coupling constraints to the `scenariobuilder.dat` file.
 
### Breaking changes  :  

* `createBindingConstraint()` is available with **offset** parameter in API mode 


# antaresEditObject 0.6.4 

BREAKING CHANGES :  

* Add UTF-8 encoding argument in `.getJobs()`
* Unit tests no longer call the study in the antaresRead package for versions > 8.0.0

BUGFIXES :  

* `createArea()`/`editArea()` : in API mode, split data in nodalOptimization argument to write it in the expected files
* `editArea()` : not delete one of the two economic options if only one must be edited
* Avoid data deletion in API mode for `editArea()`


# antaresEditObject 0.6.3

NEW FEATURES :

* Complete function `deleteStudy()` with new parameter `simulation` to delete a simulation in an Antares study.
* New parameter `geographic.trimming` in `updateGeneralSettings()`to activate or deactivate this general parameter.
* Add `importZipStudyWeb()` to allow the user to import a local study in Antares Web

### Breaking changes

* `setPlaylist()` optimized for the API mode
  - returned an updated list of simulation parameters returned by the function `setSimulationPath()` and `setSimulationPathAPI()`
* `.createCluster()` uses data.table::fwrite() instead of utils::write.table() for optimization
* `createCluster()` parameter `list_pollutants` default value to NULL.
* `createBindingConstraint()` parameter `coefficients` must be alphabetically ordered.
* `.createCluster()` default matrix in API mode.
* `removeArea()` :
  - control the existence of an area in a binding constraint coefficient before deletion
  - no longer deletes a binding constraint
* `removeLink()` : control the existence of a link a in a binding constraint coefficient before deletion
* `removeCluster()` : control the existence of a cluster a in a binding constraint coefficient before deletion
* `createClusterST()` : add a control to check if a cluster exists before running actions.
* `editClusterST()` : add a control to check if a cluster exists before running actions.
* `.removeCluster()` : add a control to check if a cluster exists before running actions in st-storage mode.
* Update documentation for scenarioBuilder : user must enable/disable `custom-scenario` property in `generaldata.ini` by himself


BUGFIXES : 

* Fix `filter_synthesis` and `filter_year_by_year` parameters of `editLink()` in API mode
* Fix `setPlaylist()` works in API and local mode with weights.
* Fix `getPlaylist()` works in API and local mode with weights.
* Fix `createDSR()` in API mode : daily binding constraint takes 366 rows.
* Fix `createCluster()` and `editCluster()` parameter `list_pollutants` stop if Antares Version < 8.6.0
* `getJobs()` no longer returns duplicates and displays the two new columns `owner_id` and `owner_name`.
* Allow the user to set symbol or full name as argument series in `updateScenarioBuilder()`
* `scenarioBuilder()` matrix has the same row repeated if the area is not rand
* Fix `createLink()` to update opts in API mode.
* Fix `editClusterST()` : can not edit a cluster if it does not exist in API mode.
* `updateScenarioBuilder()` works for NTC part : allow cartesian in the merge.
* `api_command_execute()` :  
  - no longer deletes a command  
  - displays a success message for a study or variant
* `removeCluster()` no longer deletes everything in the folder prepro  

# antaresEditObject 0.6.2
* Fix test to remove rev dep to `antaresRead` (see `cran-comments.md` for details)

# antaresEditObject 0.6.1
* `writeInputTS()` allows the user to set a link with the separator ' - ' (ex: 'area1 - area2')

BUGFIXES : 

* Error CRAN CHECKS (fix issue #115)


# antaresEditObject 0.6.0

### Breaking changes (Antares v8.6, cf. Antares v8.6 changelog) :

* `createStudy()` integrate "st-storage".
* `createArea()` integrate "st-storage".
* `removeArea()`  integrate "st-storage".
* `writeInputTS()` integrate "mingen" data and dependency between "mod.txt" and "mingen.txt" data.
* `createCluster()` / `editCluster()` have new parameter `list_pollutants` for list of pollutants.


NEW FEATURES (Antares v8.6) :

* New function `activateST()` Activate "st-storage" in an Antares study.
* New functions `createClusterST()`, `editClusterST()`, `removeClusterST()` ("st-storage" family of functions for a study in "input" mode)
* Add control of data consistency between `mingen.txt` and `mod.txt` based on values in `hydro.ini` file
* Add control of data consistency between `mingen.txt` and `maxpower.txt` based on values in `hydro.ini` file
* Add and edit the property `enable-first-step` in `adequacy patch` section in `settings/generaldata.ini`

NEW FEATURES :


* Add `deduplicateScenarioBuilder()` function to keep the last value if a key is duplicated in `settings/scenariobuilder.dat`
* Add `writeIniHydro()` function to make easier the edition of the `input/hydro/hydro.ini` file
* Call `writeIniHydro()` in `createArea()` and `removeArea()`
* Enable edition of hydro levels in `settings/scenariobuilder.dat` by using `scenarioBuilder()` and `updateScenarioBuilder()`
* Add `deduplicateScenarioBuilder()` function to keep the last value if a key is duplicated in settings/scenariobuilder.dat
* Add `writeIniHydro()` function to make easier the edition of the input/hydro/hydro.ini file
* Call `writeIniHydro()` in `createArea()` and `removeArea()`
* `removeArea()` removes only expected files in links directory


### Breaking changes

* `deleteStudy()` no longer requires user confirmation
* `api_command_execute()` displays an error message and causes the program to stop following an http error code. The error message is completed with the API error description
* `getPlaylist()` is compatible with the new format returned by `readIniAPI()`
* `removeClusterRES()` in API mode
* `removeLink()` delete properly data for an Antares version >= 820
* `rollback_to_previous_data()` enable to rollback if original value is NULL
* `writeInputTS()` allows the user to set a link with the separator ' - ' (ex: 'area1 - area2')

BUGFIXES : 

* `api_command_execute()` add timer to request api
* `writeInputTS()` works with argument `type = "tsLink"`
* `createLink()` and `editLink()` write the appropriate time series in _direct.txt and _indirect.txt files even if the areas `from` and `to` given as arguments are not sorted

### DOC : 
* A new article presenting v8.6 features is available on the package's online documentation.

# antaresEditObject 0.5.1

NEW FEATURES :

* New function `deleteStudy()` (API compatible)  
* New function `copyStudyWeb()` to import physical study into a managed study (API).
* New function `createClusterBulk()` to create multiple thermal clusters at once.
* New function `writeHydroValues()` to write hydro input files.
* Added support of ".zip" compression to existing function `backupStudy()`

BUGFIXES :

* Fixed error when using `removeArea()`.
* Fixed error when using `writeEconomicOptions()` in API mode.



# antaresEditObject 0.5.0

NEW FEATURES :

* Full support of studies up to v8.5
* New function `convertConfigToAdq()` for migration of older adequacy patch studies into v8.5  
* Existing function `updateAdequacySettings()` has new arguments (cf. Antares v8.5 changelog)
* New internal function `.createColumns()` to create headers of output data when missing.
* New function `cleanUpOutput()` to delete any extra output files not selected in simulation.
* New functions `computeOtherFromHourlyMulti()` and `computeOtherFromHourlyYear()` for mc-ind aggregation. `computeTimeStampFromHourly()` is now deprecated.

BUGFIXES :

* Major corrections to `writeOutput()` and `writeAntaresOutput()` (support V8)
* Added support of API mode for `editClusterRES()`
* Fixed ts write in API mode for `createClusterRES()`
* Fixed `getPlaylist()` and `setPlaylist()` in API mode



# antaresEditObject 0.4.1

BUGFIXES :

* Fix case sensitivity for `editCluster()` in API mode
* Fix opts returned (proper new study_id) when using `createVariant()`
* Fix case sensitivity for groups in `createCluster()` and `createClusterRES()`



# antaresEditObject 0.4.0

### New functions

* Antares v840 : new parameter `result-format` to choose output format (txt/zip) + new values for `transmission-capacities` parameter.
* Antares v832 : added filtering options to bindingConstraints.
* Antares v830 : `updateAdequacySettings()` function to activate Adequacy Patch and set parameters. + `createArea()` and `editArea()` support for new adequacy_patch.ini file.
* Antares v820 : `createLink()` and `editLink()` have a new argument `tsLink` allowing to write transmission capacities time-series.
* `createBindingConstraintBulk()` allow to create multiple binding constraints at once.


### Breaking changes

* Argument's order of `writeInputTS()` has changed, `data` is now in first place.

### Variant management with API
This release include some new features to interact with Antares Web.

* Main functions are now compatible to interact with a variant through the API: `createArea()`, `editArea()`, `removeArea()`, `createLink()`, `editLink()`, `removeLink()`, `createCluster()`, `editCluster()`, `removeCluster()`, `createBindingConstraint()`, `editBindingConstraint()`, `removeBindingConstraint()`, `createDistrict()`, `updateGeneralSettings()`, `updateInputSettings()`, `updateOptimizationSettings()`, `updateOutputSettings()`, `updateAdequacySettings()`, `writeInputTS()`, `readScenarioBuilder()`, `updateScenarioBuilder()`, `clearScenarioBuilder()`, `writeWaterValues()`, `createDSR()`, `createPSP()`.
* New functions to use or create variants: `useVariant()`, `createVariant()`.
* New function to create studies on AntaREST server: `createStudyAPI()`.
* New function to search studies on AntaREST server: `searchStudy()`.
* API calls can be synchronous or asynchronous, see `setAPImode()`.



# antaresEditObject 0.3.0

### Breaking changes
* `createStudy()` no longer return `TRUE` / `FALSE`, but simulation options list returned by `antaresRead::setSimulationPath()`.

### Renewable Energy Sources
This release include some new features to interact with renewable energy sources (new in Antares v8.0.3):

* New function `activateRES()` to use new RES features in an Antares study.
* `updateOptimizationSettings()` has a new argument `renewable.generation.modelling` that can take as value: `NULL`|`aggregated`|`cluster`.
* New function `createClusterRES()` to create a new renewable cluster.
* New function `editClusterRES()` to edit an existing renewable cluster.
* New function `removeClusterRES()` to remove a renewable cluster.
* `readScenarioBuilder()`: there can be a new field `r` if a scenario is completed for renewables
* `updateScenarioBuilder()`: allow to update scenario builder for renewables if RES activated.
* In `updateGeneralSettings()`: check value for `inter.modal` and `inter.modal` parameters:
  + RES activated: use load, hydro, thermal, renewables
  + RES not activated: use load, hydro, wind, thermal, solar
* In `updateInputSettings()`: prevent use of `renewables` for `import` parameter.

### Other updates
* `updateScenarioBuilder()` has a new argument `clusters_areas = <data.table>` to specify area/cluster to use for thermal/renewable series.
* New function `clearScenarioBuilder()` to clear a ruleset from the scenario builder.



# antaresEditObject 0.2.2

* New usage `getPlaylist` : V8 antares ponderation.
* New usage `setPlaylist` : V8 antares ponderation.



# antaresEditObject 0.2.1

* New function `copyOutput` : to copy antares output with extension.
* New function `writeOutputValues` : to write antares data after `antaresRead` (areas, links and clusters).
* New function `computeTimeStampFromHourly` : to write mc-ind data from hourly files (daily, weekly, monthly and annual).
* New function `parAggregateMCall` : to compute mc-all from md-ind. 
* New function `editArea` : to edit an area.
* New function `editBindingConstraint` : to edit a Binding Constraint.



# antaresEditObject 0.2.0

* New template in `createStudy()` for Antares version >= 7.1.0



# antaresEditObject 0.1.9

* New function `writeSeriesPrepro()`: to write load, wind and solar prepro data.
* New function `writeInputTS()`: writes input time series.
* `scenarioBuilder()` can write thermal scenario.
* New function `writeEconomicOptions()`: to create or edit economic options.
* `nodalOptimizationOptions()` has two new arguments: `average_unsupplied_energy_cost` and `average_spilled_energy_cost`.

### Bugfixes
* `scenarioBuilder()` now work when areas_rand has length 1



# antaresEditObject 0.1.8

* Internal release.



# antaresEditObject 0.1.7

* Fix `createPSP` to work with Antares v7



# antaresEditObject 0.1.6

* Compatibility with Antares version 7



# antaresEditObject 0.1.4

* Initial release.
* Added a `NEWS.md` file to track changes to the package.
