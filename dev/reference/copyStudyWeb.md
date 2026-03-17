# Import physical study to Antares Web (managed study)

Copy study from an existing workspace into a managed study. NOTE : The
study must be present in a workspace (DRD, PPSE..) not just locally.

## Usage

``` r
copyStudyWeb(
  opts = antaresRead::simOptions(),
  host,
  token,
  outputs = T,
  groups = NULL,
  suffix = "managedCopy"
)
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html).
  If id is not available, `antaresRead::searchStudy` will be used to
  find study.

- host:

  Host of AntaREST server API.

- token:

  API personnal access token.

- outputs:

  Logical. Determine if outputs are copied too.

- groups:

  Character. Add study to groups of Antares Web.

- suffix:

  Character. default is "managedCopy" By default the new study will be :
  studyname_managedCopy

## Value

New managed study ID
