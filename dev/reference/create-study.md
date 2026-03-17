# Create an empty Antares study

Create study on disk or with AntaREST server through the API.

## Usage

``` r
createStudy(path, study_name = "my_study", antares_version = "8.2.0")

createStudyAPI(
  host,
  token = NULL,
  study_name = "my_study",
  antares_version = "8.2.0",
  ...
)
```

## Arguments

- path:

  Path where to create study, it should be an empty directory, if it
  doesn't exist, it'll be created.

- study_name:

  Name of the study.

- antares_version:

  Antares number version.

- host:

  Host of AntaREST server API.

- token:

  API personnal access token.

- ...:

  Other query parameters passed to POST request.

## Value

Result of
[`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
or
[`antaresRead::setSimulationPathAPI()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)
accordingly.

## Warning

From **Antares version 9.2** onwards, versioning is only done with one
number for the major version number and a two-digit number for the minor
version number (e.g. 9.2, 9.35, 10.58, ...).

## Examples

``` r
if (FALSE) { # \dontrun{

# with default values 
createStudy("path/to/simulation", 
  study_name = "my_study", 
  antares_version = "8.2.0")
  
# with Antares study version >= 9.2 (max 2 digits, ex : "9.25")  
createStudy("path/to/simulation", 
  study_name = "my_study", 
  antares_version = "9.25")

} # }
```
