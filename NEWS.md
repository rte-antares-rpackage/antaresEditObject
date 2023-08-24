# antaresEditObject 0.6.0

### Breaking changes (Antares v8.6, cf. Antares v8.6 changelog) :

* `createStudy()` integrate "st-storage".
* `createArea()` integrate "st-storage".
* `removeArea()`  integrate "st-storage".
* `writeInputTS()` integrate "mingen" data and dependency between "mod.txt" and "mingen.txt" data.
* `createCluster()` / `editCluster()` have new parameter `list_polluants ` for list of pollutants.


NEW FEATURES (Antares v8.6) :

* New function `activateST()` Activate "st-storage" in an Antares study.
* New functions `createClusterST()`, `editClusterST()`, `removeClusterST()` ("st-storage" family of functions for a study in "input" mode)


### Breaking changes

* `deleteStudy()` no longer requires user confirmation
* `api_command_execute()` displays an error message and causes the program to stop following an http error code. The error message is completed with the API error description
* `getPlaylist()` is compatible with the new format returned by `readIniAPI()`
* `removeClusterRES()` in API mode

BUGFIXES : 

* `api_command_execute()` add timer to request api


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
