# Create a backup with an Antares Study

![Antares API NO](figures/badge_api_no.svg)

Save an Antares Study or only inputs in a `.tar.gz` or `.zip` file

## Usage

``` r
backupStudy(
  backupfile,
  what = c("study", "input"),
  compression_level = 5,
  opts = antaresRead::simOptions(),
  extension = c(".zip", ".tar.gz")
)
```

## Arguments

- backupfile:

  Name of the backup, without extension. If missing, either the name of
  the study or 'input' according argument `what`.

- what:

  Which folder to save, `input` for the input folder or `study` for the
  whole study.

- compression_level:

  "int" A number between 1 and 9 (quality of compression only used for
  `.zip` archive). See details below for more information (default to 5,
  fast and good compression).

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- extension:

  Default is `.zip`.

## Value

The path of the backup

## Details

Parameter `compression_level` is used with function
[`zip::zip()`](https://r-lib.github.io/zip/reference/zip.html)

## Examples

``` r
if (FALSE) { # \dontrun{

backupStudy()
} # }
```
