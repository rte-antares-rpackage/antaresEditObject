# Import a local study to Antares Web

Import a local study to Antares Web

## Usage

``` r
importZipStudyWeb(
  host,
  token,
  zipfile_name,
  delete_zipfile = TRUE,
  folder_destination = NULL,
  compression_level = 5,
  opts = antaresRead::simOptions()
)
```

## Arguments

- host:

  Host of AntaREST server API.

- token:

  API personnal access token.

- zipfile_name:

  Name of the zipfile of the study.

- delete_zipfile:

  Should the zipfile be deleted after upload.

- folder_destination:

  Folder of the study in Antares Web.

- compression_level:

  "int" A number between 1 and 9 (quality of compression only used for
  `.zip` archive). See details below for more information (default to 5,
  fast and good compression).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

An updated list containing various information about the simulation.

## Details

Parameter `compression_level` is used with function
[`backupStudy()`](backupStudy.md)
