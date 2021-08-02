# antaresEditObject 0.2.2.9000

This release include some new features to interact with renewable clusters:

* `updateOptimizationSettings()` has a new argument `renewable.generation.modelling` that can take as value: `NULL`|`aggregated`|`cluster`.
* New function `createClusterRenewable()` to create a new renewable cluster.
* New function `editClusterRenewable()` to edit an existing renewable cluster.
* New function `removeClusterRenewable()` to remove a renewable cluster.


# antaresEditObject 0.2.2

* New usage `getPlaylist` : V8 antares ponderation.
* New usage `setPlaylist` : V8 antares ponderation.


# antaresEditObject 0.2.1

* New function `copyOutput` : to copy antares output with extansion.
* New function `writeOutputValues` : to write antares data after `antaresRead` (areas, links and clusters).
* New function `computeTimeStampFromHourly` : to write mc-ind data from hourly files (daily, weekly, monthly and annual).
* New function `parAggregateMCall` : to compute mc-all from md-ind. 
* New function `editArea` : to edit area
* New function `editBindingConstraint` : to edit Binding Constraint


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
