# Create a backup with an Antares Study

![Antares API NO](figures/badge_api_no.svg)

Save an Antares Study or only inputs in a `.tar.gz` or `.zip` file

## Usage

``` r
backupStudy(
  backupfile,
  what = "study",
  opts = antaresRead::simOptions(),
  extension = ".zip"
)
```

## Arguments

- backupfile:

  Name of the backup, without extension. If missing, either the name of
  the study or 'input' according argument `what`.

- what:

  Which folder to save, `input` for the input folder or `study` for the
  whole study.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- extension:

  Defaut is `.zip`.

## Value

The path of the backup

## Examples

``` r
if (FALSE) { # \dontrun{
backupStudy()
} # }
```
