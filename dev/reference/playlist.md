# Get the playlist of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

`getPlaylist` gives the identifier of the MC years which will be
simulated in the Antares study, taking into account the potential use of
a playlist which can skip some MC years

`setPlaylist` is a function which modifies the input file of an ANTARES
study and set the playlist in order to simulate only the MC years given
in input

## Usage

``` r
getPlaylist(opts = antaresRead::simOptions())

setPlaylist(playlist, weights = NULL, opts = antaresRead::simOptions())
```

## Arguments

- opts:

  List of simulation parameters returned by the function
  [`antaresRead::setSimulationPath()`](https://rte-antares-rpackage.github.io/antaresRead/reference/setSimulationPath.html)

- playlist:

  vector of MC years identifier to be simulated can be a list (V8
  compatibility) but not recommended

- weights:

  data.table, 2 columns : mcYears and weights. Only with after antares
  V8

## Value

- `getPlaylist` returns a vector of the identifier of the simulated MC
  year.

&nbsp;

- `setPlaylist` does not return anything. It is used to modify the input
  of an Antares study.

## Examples

``` r
if (FALSE) { # \dontrun{
setSimulationPath("PATH/TO/STUDY/")
# or 
setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "6f98a393-155d-450f-a581-8668dc355235",
  token = NULL,
  simulation = "input"
)

# augment number of MC years
updateGeneralSettings(nbyears = 10)

# Get the actual playlist
getPlaylist()
# [1] 2 4 6

# set a new playlist
setPlaylist(c(3, 5, 7))
} # }
```
