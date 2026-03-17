# Get API commands generated

Get API commands generated

## Usage

``` r
getVariantCommands(
  last = NULL,
  actions = NULL,
  opts = antaresRead::simOptions()
)

writeVariantCommands(
  path,
  last = NULL,
  actions = NULL,
  ...,
  opts = antaresRead::simOptions()
)
```

## Arguments

- last:

  Return the last command generated if `TRUE`, or a `numeric` for
  returning a specified number of commands. Default is to return all
  commands.

- actions:

  A `character` `vector` of actions to return.

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- path:

  Path to the JSON file to write on disk.

- ...:

  Additional arguments passed to
  [`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)

## Value

a list of commands to edit a variant
