# Search study in AntaREST

Search study in AntaREST

## Usage

``` r
searchStudy(
  workspace = NULL,
  folder = NULL,
  name = NULL,
  ...,
  host = NULL,
  token = NULL
)
```

## Arguments

- workspace:

  Space in which to search for a study.

- folder:

  Folder in which to search for a study.

- name:

  Name for the study.

- ...:

  Other query parameters.

- host:

  Host of AntaREST server API.

- token:

  API personnal access token.

## Value

a `data.table` with informations about studies on the server.

## Examples

``` r
if (FALSE) { # \dontrun{

searchStudies(host = "http://localhost:8080")

} # }
```
