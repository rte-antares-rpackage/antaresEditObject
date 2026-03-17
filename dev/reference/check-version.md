# Is study an Antares v7 study ?

Is study an Antares v7 study ?

## Usage

``` r
is_antares_v7(opts = antaresRead::simOptions())

is_antares_v820(opts = antaresRead::simOptions())
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

## Value

a logical, `TRUE` if study is v7 or above, `FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# setSimulationPath

is_antares_v7()

} # }
```
