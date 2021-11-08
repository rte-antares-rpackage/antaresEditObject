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
