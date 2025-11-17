# Write configuration options in file or API

Write configuration options in file or API

## Usage

``` r
writeIni(
  listData,
  pathIni,
  opts = antaresRead::simOptions(),
  ...,
  default_ext = ".ini"
)

writeIniFile(listData, pathIni, overwrite = FALSE)

writeIniAPI(listData, pathIni, opts)
```

## Arguments

- listData:

  `list`, modified list obtained by antaresRead:::readIniFile.

- pathIni:

  `Character`, Path to ini file.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- ...:

  Additional arguments.

- default_ext:

  Default extension used for config files.

- overwrite:

  logical, should file be overwritten if already exist?

## Examples

``` r
if (FALSE) { # \dontrun{
pathIni <- "D:/exemple_test/settings/generaldata.ini"
generalSetting <- readIniFile(pathIni)
generalSetting$output$synthesis <- FALSE
writeIni(generalSetting, pathIni)
} # }
```
