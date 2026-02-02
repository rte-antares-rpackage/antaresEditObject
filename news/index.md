# Changelog

## antaresEditObject 0.9.4.9000

NEW FEATURES :

- [`createDistrict()`](../reference/createDistrict.md) uses a specific
  endpoint to create the district
- [`editDistrict()`](../reference/editDistrict.md) allows the user to
  edit a district. Use of a specific endpoint in API mode.

ENHANCEMENT :

- [`createBindingConstraint()`](../reference/createBindingConstraint.md)/`editBindingCOnstraint()`
  : remove depency to
  [`readClusterDesc()`](https://rte-antares-rpackage.github.io/antaresRead/reference/readClusterDesc.html)
  from antaresRead package

BUGFIXES :

- `.get_version_solver_from_path_solver()` get solver version by running
  command line instead of searching in the path.

OPTIMIZATION :

- [`createBindingConstraintBulk()`](../reference/createBindingConstraintBulk.md)
  reads links and clusters of the study once

## antaresEditObject 0.9.3

CRAN release: 2025-11-17

(cf. Antares v9.3 changelog)

NEW FEATURES :

- [`createClusterST()`](../reference/createClusterST.md)/[`editClusterST()`](../reference/editClusterST.md)
  :
  - **New properties** (*allow-overflow*, see list of properties
    according to study version of Antares with
    [`storage_values_default()`](../reference/storage_values_default.md))
- [`createClusterST()`](../reference/createClusterST.md)/[`editClusterST()`](../reference/editClusterST.md)
  :
  - **New dimension of time series** (all matrices will be of (8760, N),
    noting that N \>= 1)
  - **Suppression of properties**
    (refreshtimeseries,refreshintervalload, refreshintervalhydro,
    refreshintervalwind, refreshintervalthermal, refreshintervalsolar
    from settings/generaldata.ini)
- [`createCluster()`](../reference/createCluster.md) :
  - **Dynamic groups** (The group parameter is now dynamic and has no
    restrictions. The default group value for Renewable depends on the
    version: Other RES 1 if \< 930, Other if \>= 930).
- [`updateScenarioBuilder()`](../reference/scenario-builder.md) New
  types of series “sts” and “sta” (“sct apports level” and “sct
  contraintes”) is available

BUGFIXES :

- `createBindingConstraint()/editBindingConstraint()` do not allow to
  create/edit a binding constraint if a cluster referenced in a
  coefficient does not exist.

## antaresEditObject 0.9.2.9000

(cf. Antares v9.2 changelog)

NEW FEATURES :

- [`createStudy()`](../reference/create-study.md) initializes the study
  by updating the `generaldata.ini` file :
  - the value of the `shedding-policy` parameter is changed to “accurate
    shave peaks”
  - a new “compatibility” section is created with parameter `hydro-pmax`
    = “daily”
- [`createArea()`](../reference/createArea.md) initializes the
  `hydro.ini` file with a new parameter
  `overflow spilled cost difference` (for each area)
- [`createClusterST()`](../reference/createClusterST.md)/[`editClusterST()`](../reference/editClusterST.md)
  : Parameter `group` is now dynamic and have no restriction
- [`createClusterST()`](../reference/createClusterST.md)/[`editClusterST()`](../reference/editClusterST.md)
  :
  - **New properties** (*efficiencywithdrawal*,
    *penalize-variation-injection*, *penalize-variation-withdrawal*, see
    list of properties according to study version of Antares with
    [`storage_values_default()`](../reference/storage_values_default.md))  
  - **New optional time series** (cost-injection, cost-withdrawal,
    cost-level, cost-variation-injection, cost-variation-withdrawal)
  - **New additional constraints** (properties and time series)  
- [`removeClusterST()`](../reference/removeCluster.md) : remove **New
  optional time series** + **New additional constraints**  
- [`updateScenarioBuilder()`](../reference/scenario-builder.md) New type
  of series “hfl” (“hydro final level”, similar to “hydrolevels”) is
  available
- [`removeDistrict()`](../reference/removeDistrict.md) remove a district
  from a study
- [`createArea()`](../reference/createArea.md)/[`editArea()`](../reference/editArea.md):
  in API mode, allow the user to customize localization and color of an
  area

NEW FEATURES (other) :

- [`editBindingConstraint()`](../reference/editBindingConstraint.md) :
  control the dimensions of the matrix only if a time series is provided
  by the user for optimization
- `.createCluster()` uses a specific endpoint to write cluster’s
  metadata and commands to write matrix
- Add new function
  [`setThematicTrimming()`](../reference/setThematicTrimming.md) to set
  the thematic trimming in file `generaldata.ini`
- [`createStudy()`](../reference/create-study.md)\* adds `author` in
  `study.antares` file

#### Breaking changes :

- [`createClusterST()`](../reference/createClusterST.md) : For a study
  \< *v9.2*, execution will be STOP if `group` is not included in list
  (see doc)  
- [`updateAdequacySettings()`](../reference/updateAdequacySettings.md) :
  Two parameters (*enable-first-step*,
  *set-to-null-ntc-between-physical-out-for-first-step*) are
  **deprecated** and removed. Parameters are forced to `NULL` with study
  \>= v9.2.

BUGFIXES :

- [`clearScenarioBuilder()`](../reference/scenario-builder.md) in API
  mode, updates correctly with empty data
  ([`{}`](https://rdrr.io/r/base/Paren.html))

#### DOC :

A new article exposing new features of Antares Simulator v9.2 is
available
[here](https://rte-antares-rpackage.github.io/antaresEditObject/dev/articles/Antares_new_features_v920.html)

## antaresEditObject 0.9.0

CRAN release: 2025-02-14

(cf. Antares v9 changelog)

NEW FEATURES (Antares v9.0) :

- [`createStudy()`](../reference/create-study.md) takes into account the
  new format of Antares studies (e.g. 9.0, 9.15 instead of 900, 915)

BUGFIXES :

- [`editBindingConstraint()`](../reference/editBindingConstraint.md) :
  - `operator` parameter set to NULL, by default, no longer causes an
    error.  
  - To add values, the `operator` parameter is now required.  
  - For a study version \>= 832, the `filter-year-by-year` and
    `filter-synthesis` properties are retained in the .ini file if they
    are not modified.  
- *\[private function\]* `api_command_execute()` manage snapshot
  generation of a variant study with a tempo to wait the end of current
  task (prevents the order from being ignored).
  - You can use global parameter `verbose` to `TRUE`
    ([\#274](https://github.com/rte-antares-rpackage/antaresRead/pull/274)
    `antaresRead`) to display diagnostic messages
    (`getOption("antares")`)
- [`updateAdequacySettings()`](../reference/updateAdequacySettings.md) :
  in API mode do not send NULL value

GITHUB ACTIONS :

- Actions artifacts v3 is closing down, update to v4  
- test-coverage.yaml updated

## antaresEditObject 0.7.1

CRAN release: 2024-09-27

#### Breaking changes :

- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  / [`editBindingConstraint()`](../reference/editBindingConstraint.md)
  uses metadata to check the group size of time series.
- [`createBindingConstraintBulk()`](../reference/createBindingConstraintBulk.md)
  checks consistency of groups passed as parameters and consistency with
  the study.
- [`importZipStudyWeb()`](../reference/importZipStudyWeb.md) can delete
  the zipfile and move the study in Antares Web to another folder
- delete `antaresRead-reexports.R` and adjust scripts to have a clean
  package
- [`removeArea()`](../reference/removeArea.md) : send a warning instead
  of a stop if an area is referenced in a binding constraint coefficient
- [`removeLink()`](../reference/removeLink.md) : send a warning instead
  of a stop if a link is referenced in a binding constraint coefficient
- [`removeCluster()`](../reference/removeCluster.md) : send a warning
  instead of a stop if a cluster is referenced in a binding constraint
  coefficient
- [`createClusterST()`](../reference/createClusterST.md) : updated with
  new endpoint API (POST + PUT)
- [`editClusterST()`](../reference/editClusterST.md) : updated with new
  endpoint API (PATCH + PUT)
- [`removeCluster()`](../reference/removeCluster.md)/[`removeClusterRES()`](../reference/removeCluster.md)/[`removeClusterST()`](../reference/removeCluster.md)
  updated with new endpoint API (DELETE)

NEW FEATURES (Antares v8.8) :

- [`updateOptimizationSettings()`](../reference/updateOptimizationSettings.md)
  allows the user to update solver.log property  
- [`createClusterST()`](../reference/createClusterST.md) /
  [`editClusterST()`](../reference/editClusterST.md) use new parameters
  and default values
- Add new function [`api_patch()`](../reference/api_patch.md) to put
  PATCH (httr) request to API

BUGFIXES :

- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  in API mode (for study \<v870) created with “hourly” timeStep all the
  time
- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  / [`editBindingConstraint()`](../reference/editBindingConstraint.md)
  in TEXT mode, bad values in time series  
- [`createBindingConstraintBulk()`](../reference/createBindingConstraintBulk.md)
  with no VALUES and with a mix
- Enable control of matrix dimension in `.check_bulk_object_dim()` even
  if the values are not in first position in the list
- [`editLink()`](../reference/editLink.md) : avoid *NULL* value
  (default) for arguments *filter_synthesis* and *filter_year_by_year*
  to write an empty string
- [`updateOutputSettings()`](../reference/updateOutputSettings.md) : in
  API mode, allow the user to edit the desired property
- [`setPlaylist()`](../reference/playlist.md): do not send NULL value if
  the weights are not provided in argument

OTHER UPDATES :

- [`updateGeneralSettings()`](../reference/updateGeneralSettings.md) :
  replace custom.ts.numbers argument by custom.scenario and deprecate
  custom.ts.numbers  
- [`updateGeneralSettings()`](../reference/updateGeneralSettings.md) :
  add thematic.trimming argument for edition

DOC :

- [`createClusterST()`](../reference/createClusterST.md) : update doc to
  discrabe st-storage list parameters + “Inflows” parameter

## antaresEditObject 0.7.0

CRAN release: 2024-05-30

> Scenarized RHS for binding constraints

NEW FEATURES (Antares v8.7, cf. Antares v8.7 changelog) :

- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  /
  [`createBindingConstraintBulk()`](../reference/createBindingConstraintBulk.md)
  - New parameters `group`  
  - Parameter `values` is now list of `data.frame`
- [`editBindingConstraint()`](../reference/editBindingConstraint.md)
  - New parameters `group`  
  - Parameter `values` is now list of `data.frame`
- [`removeBindingConstraint()`](../reference/removeBindingConstraint.md)
  can now delete coupling constraints from the `group` parameter.
- [`scenarioBuilder()`](../reference/scenario-builder.md) has 3 new
  parameters dedicated to use with binding constraints.
- [`updateGeneralSettings()`](../reference/updateGeneralSettings.md)
  adds coupling constraints to the `scenariobuilder.dat` file.

#### Breaking changes :

- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  is available with **offset** parameter in API mode

## antaresEditObject 0.6.4

CRAN release: 2024-05-24

BREAKING CHANGES :

- Add UTF-8 encoding argument in `.getJobs()`
- Unit tests no longer call the study in the antaresRead package for
  versions \> 8.0.0

BUGFIXES :

- [`createArea()`](../reference/createArea.md)/[`editArea()`](../reference/editArea.md)
  : in API mode, split data in nodalOptimization argument to write it in
  the expected files
- [`editArea()`](../reference/editArea.md) : not delete one of the two
  economic options if only one must be edited
- Avoid data deletion in API mode for
  [`editArea()`](../reference/editArea.md)

## antaresEditObject 0.6.3

CRAN release: 2024-04-29

NEW FEATURES :

- Complete function [`deleteStudy()`](../reference/deleteStudy.md) with
  new parameter `simulation` to delete a simulation in an Antares study.
- New parameter `geographic.trimming` in
  [`updateGeneralSettings()`](../reference/updateGeneralSettings.md)to
  activate or deactivate this general parameter.
- Add [`importZipStudyWeb()`](../reference/importZipStudyWeb.md) to
  allow the user to import a local study in Antares Web

#### Breaking changes

- [`setPlaylist()`](../reference/playlist.md) optimized for the API mode
  - returned an updated list of simulation parameters returned by the
    function
    [`setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
    and
    [`setSimulationPathAPI()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
- `.createCluster()` uses data.table::fwrite() instead of
  utils::write.table() for optimization
- [`createCluster()`](../reference/createCluster.md) parameter
  `list_pollutants` default value to NULL.
- [`createBindingConstraint()`](../reference/createBindingConstraint.md)
  parameter `coefficients` must be alphabetically ordered.
- `.createCluster()` default matrix in API mode.
- [`removeArea()`](../reference/removeArea.md) :
  - control the existence of an area in a binding constraint coefficient
    before deletion
  - no longer deletes a binding constraint
- [`removeLink()`](../reference/removeLink.md) : control the existence
  of a link a in a binding constraint coefficient before deletion
- [`removeCluster()`](../reference/removeCluster.md) : control the
  existence of a cluster a in a binding constraint coefficient before
  deletion
- [`createClusterST()`](../reference/createClusterST.md) : add a control
  to check if a cluster exists before running actions.
- [`editClusterST()`](../reference/editClusterST.md) : add a control to
  check if a cluster exists before running actions.
- `.removeCluster()` : add a control to check if a cluster exists before
  running actions in st-storage mode.
- Update documentation for scenarioBuilder : user must enable/disable
  `custom-scenario` property in `generaldata.ini` by himself

BUGFIXES :

- Fix `filter_synthesis` and `filter_year_by_year` parameters of
  [`editLink()`](../reference/editLink.md) in API mode
- Fix [`setPlaylist()`](../reference/playlist.md) works in API and local
  mode with weights.
- Fix [`getPlaylist()`](../reference/playlist.md) works in API and local
  mode with weights.
- Fix [`createDSR()`](../reference/createDSR.md) in API mode : daily
  binding constraint takes 366 rows.
- Fix [`createCluster()`](../reference/createCluster.md) and
  [`editCluster()`](../reference/editCluster.md) parameter
  `list_pollutants` stop if Antares Version \< 8.6.0
- [`getJobs()`](../reference/getJobs.md) no longer returns duplicates
  and displays the two new columns `owner_id` and `owner_name`.
- Allow the user to set symbol or full name as argument series in
  [`updateScenarioBuilder()`](../reference/scenario-builder.md)
- [`scenarioBuilder()`](../reference/scenario-builder.md) matrix has the
  same row repeated if the area is not rand
- Fix [`createLink()`](../reference/createLink.md) to update opts in API
  mode.
- Fix [`editClusterST()`](../reference/editClusterST.md) : can not edit
  a cluster if it does not exist in API mode.
- [`updateScenarioBuilder()`](../reference/scenario-builder.md) works
  for NTC part : allow cartesian in the merge.
- `api_command_execute()` :
  - no longer deletes a command  
  - displays a success message for a study or variant
- [`removeCluster()`](../reference/removeCluster.md) no longer deletes
  everything in the folder prepro

## antaresEditObject 0.6.2

CRAN release: 2024-04-19

- Fix test to remove rev dep to `antaresRead` (see `cran-comments.md`
  for details)

## antaresEditObject 0.6.1

CRAN release: 2023-12-12

- [`writeInputTS()`](../reference/writeInputTS.md) allows the user to
  set a link with the separator ’ - ’ (ex: ‘area1 - area2’)

BUGFIXES :

- Error CRAN CHECKS (fix issue
  [\#115](https://github.com/rte-antares-rpackage/antaresEditObject/issues/115))

## antaresEditObject 0.6.0

CRAN release: 2023-10-03

#### Breaking changes (Antares v8.6, cf. Antares v8.6 changelog) :

- [`createStudy()`](../reference/create-study.md) integrate
  “st-storage”.
- [`createArea()`](../reference/createArea.md) integrate “st-storage”.
- [`removeArea()`](../reference/removeArea.md) integrate “st-storage”.
- [`writeInputTS()`](../reference/writeInputTS.md) integrate “mingen”
  data and dependency between “mod.txt” and “mingen.txt” data.
- [`createCluster()`](../reference/createCluster.md) /
  [`editCluster()`](../reference/editCluster.md) have new parameter
  `list_pollutants` for list of pollutants.

NEW FEATURES (Antares v8.6) :

- New function [`activateST()`](../reference/activateST.md) Activate
  “st-storage” in an Antares study.
- New functions [`createClusterST()`](../reference/createClusterST.md),
  [`editClusterST()`](../reference/editClusterST.md),
  [`removeClusterST()`](../reference/removeCluster.md) (“st-storage”
  family of functions for a study in “input” mode)
- Add control of data consistency between `mingen.txt` and `mod.txt`
  based on values in `hydro.ini` file
- Add control of data consistency between `mingen.txt` and
  `maxpower.txt` based on values in `hydro.ini` file
- Add and edit the property `enable-first-step` in `adequacy patch`
  section in `settings/generaldata.ini`

NEW FEATURES :

- Add [`deduplicateScenarioBuilder()`](../reference/scenario-builder.md)
  function to keep the last value if a key is duplicated in
  `settings/scenariobuilder.dat`
- Add [`writeIniHydro()`](../reference/writeIniHydro.md) function to
  make easier the edition of the `input/hydro/hydro.ini` file
- Call [`writeIniHydro()`](../reference/writeIniHydro.md) in
  [`createArea()`](../reference/createArea.md) and
  [`removeArea()`](../reference/removeArea.md)
- Enable edition of hydro levels in `settings/scenariobuilder.dat` by
  using [`scenarioBuilder()`](../reference/scenario-builder.md) and
  [`updateScenarioBuilder()`](../reference/scenario-builder.md)
- Add [`deduplicateScenarioBuilder()`](../reference/scenario-builder.md)
  function to keep the last value if a key is duplicated in
  settings/scenariobuilder.dat
- Add [`writeIniHydro()`](../reference/writeIniHydro.md) function to
  make easier the edition of the input/hydro/hydro.ini file
- Call [`writeIniHydro()`](../reference/writeIniHydro.md) in
  [`createArea()`](../reference/createArea.md) and
  [`removeArea()`](../reference/removeArea.md)
- [`removeArea()`](../reference/removeArea.md) removes only expected
  files in links directory

#### Breaking changes

- [`deleteStudy()`](../reference/deleteStudy.md) no longer requires user
  confirmation
- `api_command_execute()` displays an error message and causes the
  program to stop following an http error code. The error message is
  completed with the API error description
- [`getPlaylist()`](../reference/playlist.md) is compatible with the new
  format returned by
  [`readIniAPI()`](https://rte-antares-rpackage.github.io/antaresRead/reference/read-ini.html)
- [`removeClusterRES()`](../reference/removeCluster.md) in API mode
- [`removeLink()`](../reference/removeLink.md) delete properly data for
  an Antares version \>= 820
- [`rollback_to_previous_data()`](../reference/rollback_to_previous_data.md)
  enable to rollback if original value is NULL
- [`writeInputTS()`](../reference/writeInputTS.md) allows the user to
  set a link with the separator ’ - ’ (ex: ‘area1 - area2’)

BUGFIXES :

- `api_command_execute()` add timer to request api
- [`writeInputTS()`](../reference/writeInputTS.md) works with argument
  `type = "tsLink"`
- [`createLink()`](../reference/createLink.md) and
  [`editLink()`](../reference/editLink.md) write the appropriate time
  series in \_direct.txt and \_indirect.txt files even if the areas
  `from` and `to` given as arguments are not sorted

#### DOC :

- A new article presenting v8.6 features is available on the package’s
  online documentation.

## antaresEditObject 0.5.1

CRAN release: 2023-04-06

NEW FEATURES :

- New function [`deleteStudy()`](../reference/deleteStudy.md) (API
  compatible)  
- New function [`copyStudyWeb()`](../reference/copyStudyWeb.md) to
  import physical study into a managed study (API).
- New function
  [`createClusterBulk()`](../reference/createClusterBulk.md) to create
  multiple thermal clusters at once.
- New function [`writeHydroValues()`](../reference/writeHydroValues.md)
  to write hydro input files.
- Added support of “.zip” compression to existing function
  [`backupStudy()`](../reference/backupStudy.md)

BUGFIXES :

- Fixed error when using [`removeArea()`](../reference/removeArea.md).
- Fixed error when using
  [`writeEconomicOptions()`](../reference/writeEconomicOptions.md) in
  API mode.

## antaresEditObject 0.5.0

CRAN release: 2023-03-10

NEW FEATURES :

- Full support of studies up to v8.5
- New function
  [`convertConfigToAdq()`](../reference/convertConfigToAdq.md) for
  migration of older adequacy patch studies into v8.5  
- Existing function
  [`updateAdequacySettings()`](../reference/updateAdequacySettings.md)
  has new arguments (cf. Antares v8.5 changelog)
- New internal function `.createColumns()` to create headers of output
  data when missing.
- New function [`cleanUpOutput()`](../reference/cleanUpOutput.md) to
  delete any extra output files not selected in simulation.
- New functions
  [`computeOtherFromHourlyMulti()`](../reference/computeOtherFromHourlyMulti.md)
  and
  [`computeOtherFromHourlyYear()`](../reference/computeOtherFromHourlyYear.md)
  for mc-ind aggregation.
  [`computeTimeStampFromHourly()`](../reference/computeTimeStampFromHourly.md)
  is now deprecated.

BUGFIXES :

- Major corrections to `writeOutput()` and `writeAntaresOutput()`
  (support V8)
- Added support of API mode for
  [`editClusterRES()`](../reference/editCluster.md)
- Fixed ts write in API mode for
  [`createClusterRES()`](../reference/createCluster.md)
- Fixed [`getPlaylist()`](../reference/playlist.md) and
  [`setPlaylist()`](../reference/playlist.md) in API mode

## antaresEditObject 0.4.1

BUGFIXES :

- Fix case sensitivity for
  [`editCluster()`](../reference/editCluster.md) in API mode
- Fix opts returned (proper new study_id) when using
  [`createVariant()`](../reference/variant.md)
- Fix case sensitivity for groups in
  [`createCluster()`](../reference/createCluster.md) and
  [`createClusterRES()`](../reference/createCluster.md)

## antaresEditObject 0.4.0

CRAN release: 2022-12-06

#### New functions

- Antares v840 : new parameter `result-format` to choose output format
  (txt/zip) + new values for `transmission-capacities` parameter.
- Antares v832 : added filtering options to bindingConstraints.
- Antares v830 :
  [`updateAdequacySettings()`](../reference/updateAdequacySettings.md)
  function to activate Adequacy Patch and set parameters. +
  [`createArea()`](../reference/createArea.md) and
  [`editArea()`](../reference/editArea.md) support for new
  adequacy_patch.ini file.
- Antares v820 : [`createLink()`](../reference/createLink.md) and
  [`editLink()`](../reference/editLink.md) have a new argument `tsLink`
  allowing to write transmission capacities time-series.
- [`createBindingConstraintBulk()`](../reference/createBindingConstraintBulk.md)
  allow to create multiple binding constraints at once.

#### Breaking changes

- Argument’s order of [`writeInputTS()`](../reference/writeInputTS.md)
  has changed, `data` is now in first place.

#### Variant management with API

This release include some new features to interact with Antares Web.

- Main functions are now compatible to interact with a variant through
  the API: [`createArea()`](../reference/createArea.md),
  [`editArea()`](../reference/editArea.md),
  [`removeArea()`](../reference/removeArea.md),
  [`createLink()`](../reference/createLink.md),
  [`editLink()`](../reference/editLink.md),
  [`removeLink()`](../reference/removeLink.md),
  [`createCluster()`](../reference/createCluster.md),
  [`editCluster()`](../reference/editCluster.md),
  [`removeCluster()`](../reference/removeCluster.md),
  [`createBindingConstraint()`](../reference/createBindingConstraint.md),
  [`editBindingConstraint()`](../reference/editBindingConstraint.md),
  [`removeBindingConstraint()`](../reference/removeBindingConstraint.md),
  [`createDistrict()`](../reference/createDistrict.md),
  [`updateGeneralSettings()`](../reference/updateGeneralSettings.md),
  [`updateInputSettings()`](../reference/updateInputSettings.md),
  [`updateOptimizationSettings()`](../reference/updateOptimizationSettings.md),
  [`updateOutputSettings()`](../reference/updateOutputSettings.md),
  [`updateAdequacySettings()`](../reference/updateAdequacySettings.md),
  [`writeInputTS()`](../reference/writeInputTS.md),
  [`readScenarioBuilder()`](../reference/scenario-builder.md),
  [`updateScenarioBuilder()`](../reference/scenario-builder.md),
  [`clearScenarioBuilder()`](../reference/scenario-builder.md),
  [`writeWaterValues()`](../reference/writeWaterValues.md),
  [`createDSR()`](../reference/createDSR.md),
  [`createPSP()`](../reference/createPSP.md).
- New functions to use or create variants:
  [`useVariant()`](../reference/variant.md),
  [`createVariant()`](../reference/variant.md).
- New function to create studies on AntaREST server:
  [`createStudyAPI()`](../reference/create-study.md).
- New function to search studies on AntaREST server:
  [`searchStudy()`](../reference/searchStudy.md).
- API calls can be synchronous or asynchronous, see
  [`setAPImode()`](../reference/setAPImode.md).

## antaresEditObject 0.3.0

CRAN release: 2021-11-08

#### Breaking changes

- [`createStudy()`](../reference/create-study.md) no longer return
  `TRUE` / `FALSE`, but simulation options list returned by
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).

#### Renewable Energy Sources

This release include some new features to interact with renewable energy
sources (new in Antares v8.0.3):

- New function [`activateRES()`](../reference/activateRES.md) to use new
  RES features in an Antares study.
- [`updateOptimizationSettings()`](../reference/updateOptimizationSettings.md)
  has a new argument `renewable.generation.modelling` that can take as
  value: `NULL`\|`aggregated`\|`cluster`.
- New function [`createClusterRES()`](../reference/createCluster.md) to
  create a new renewable cluster.
- New function [`editClusterRES()`](../reference/editCluster.md) to edit
  an existing renewable cluster.
- New function [`removeClusterRES()`](../reference/removeCluster.md) to
  remove a renewable cluster.
- [`readScenarioBuilder()`](../reference/scenario-builder.md): there can
  be a new field `r` if a scenario is completed for renewables
- [`updateScenarioBuilder()`](../reference/scenario-builder.md): allow
  to update scenario builder for renewables if RES activated.
- In [`updateGeneralSettings()`](../reference/updateGeneralSettings.md):
  check value for `inter.modal` and `inter.modal` parameters:
  - RES activated: use load, hydro, thermal, renewables
  - RES not activated: use load, hydro, wind, thermal, solar
- In [`updateInputSettings()`](../reference/updateInputSettings.md):
  prevent use of `renewables` for `import` parameter.

#### Other updates

- [`updateScenarioBuilder()`](../reference/scenario-builder.md) has a
  new argument `clusters_areas = <data.table>` to specify area/cluster
  to use for thermal/renewable series.
- New function
  [`clearScenarioBuilder()`](../reference/scenario-builder.md) to clear
  a ruleset from the scenario builder.

## antaresEditObject 0.2.2

CRAN release: 2021-06-21

- New usage `getPlaylist` : V8 antares ponderation.
- New usage `setPlaylist` : V8 antares ponderation.

## antaresEditObject 0.2.1

- New function `copyOutput` : to copy antares output with extension.
- New function `writeOutputValues` : to write antares data after
  `antaresRead` (areas, links and clusters).
- New function `computeTimeStampFromHourly` : to write mc-ind data from
  hourly files (daily, weekly, monthly and annual).
- New function `parAggregateMCall` : to compute mc-all from md-ind.
- New function `editArea` : to edit an area.
- New function `editBindingConstraint` : to edit a Binding Constraint.

## antaresEditObject 0.2.0

- New template in [`createStudy()`](../reference/create-study.md) for
  Antares version \>= 7.1.0

## antaresEditObject 0.1.9

CRAN release: 2020-02-20

- New function
  [`writeSeriesPrepro()`](../reference/writeSeriesPrepro.md): to write
  load, wind and solar prepro data.
- New function [`writeInputTS()`](../reference/writeInputTS.md): writes
  input time series.
- [`scenarioBuilder()`](../reference/scenario-builder.md) can write
  thermal scenario.
- New function
  [`writeEconomicOptions()`](../reference/writeEconomicOptions.md): to
  create or edit economic options.
- [`nodalOptimizationOptions()`](../reference/nodalOptimizationOptions.md)
  has two new arguments: `average_unsupplied_energy_cost` and
  `average_spilled_energy_cost`.

#### Bugfixes

- [`scenarioBuilder()`](../reference/scenario-builder.md) now work when
  areas_rand has length 1

## antaresEditObject 0.1.8

- Internal release.

## antaresEditObject 0.1.7

CRAN release: 2019-07-18

- Fix `createPSP` to work with Antares v7

## antaresEditObject 0.1.6

CRAN release: 2019-05-13

- Compatibility with Antares version 7

## antaresEditObject 0.1.4

- Initial release.
- Added a `NEWS.md` file to track changes to the package.
